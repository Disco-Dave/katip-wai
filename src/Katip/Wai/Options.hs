module Katip.Wai.Options
  ( -- * Formatting
    Formatter
  , TimeUnit (..)
  , IncludedHeaders
  , defaultIncludeHeaders
  , defaultRequestFormat
  , defaultResponseFormat

    -- * Options
  , Options (..)
  , options
  , defaultOptions
  )
where

import Katip.Wai.Request (Request)
import qualified Katip.Wai.Request as Request
import Katip.Wai.Response (Response)
import qualified Katip.Wai.Response as Response

import qualified Data.Aeson as Aeson
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import qualified Data.CaseInsensitive as CI
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Encoding.Error as TextEncodingError
import qualified Data.UUID as UUID
import qualified Katip
import qualified Network.HTTP.Types as HttpTypes
import qualified System.Clock as Clock


-- * Formatting


type Formatter a = a -> Aeson.Value


data TimeUnit
  = Nanoseconds
  | Microseconds
  | Milliseconds
  | Seconds
  deriving (Show, Eq, Ord, Enum, Bounded)


type IncludedHeaders = Set HttpTypes.HeaderName


bsToText :: ByteString -> Text
bsToText =
  TextEncoding.decodeUtf8With TextEncodingError.lenientDecode


filterHeaders :: IncludedHeaders -> [HttpTypes.Header] -> [HttpTypes.Header]
filterHeaders includedHeaders =
  filter (flip Set.member includedHeaders . fst)


formatHeaders :: [HttpTypes.Header] -> Aeson.Value
formatHeaders headers =
  Aeson.toJSON . Map.fromList $ fmap (bimap (CI.original . CI.map bsToText) bsToText) headers


formatTimeSpec :: TimeUnit -> Clock.TimeSpec -> Aeson.Value
formatTimeSpec timeUnit timeSpec =
  let
    (abbreviation, divisor) =
      case timeUnit of
        Nanoseconds -> ("ns" :: Text, 1 :: Double)
        Microseconds -> ("μs", 1e+3)
        Milliseconds -> ("ms", 1e+6)
        Seconds -> ("s", 1e+9)
   in
    Aeson.object
      [ "unit" Aeson..= abbreviation
      , "time" Aeson..= (fromIntegral (Clock.toNanoSecs timeSpec) / divisor)
      ]


defaultIncludeHeaders :: IncludedHeaders
defaultIncludeHeaders =
  Set.fromList
    [ "Host"
    , "Referer"
    , "User-Agent"
    , "Range"
    ]


defaultRequestFormat :: IncludedHeaders -> Formatter Request
defaultRequestFormat includedHeaders request =
  Aeson.object
    [ "traceId" Aeson..= UUID.toText (Request.traceId request)
    , "method" Aeson..= bsToText (Request.method request)
    , "httpVersion" Aeson..= show (Request.httpVersion request)
    , "rawPathInfo" Aeson..= bsToText (Request.rawPathInfo request)
    , "headers" Aeson..= formatHeaders (filterHeaders includedHeaders (Request.requestHeaders request))
    , "isSecure" Aeson..= Request.isSecure request
    , "remoteHost" Aeson..= show (Request.remoteHost request)
    , "pathInfo" Aeson..= Request.pathInfo request
    , "queryString" Aeson..= fmap (bimap bsToText (fmap bsToText)) (Request.queryString request)
    , "receivedAt" Aeson..= Request.receivedAt request
    ]


defaultResponseFormat :: IncludedHeaders -> TimeUnit -> Formatter Response
defaultResponseFormat includedHeaders timeUnit response =
  Aeson.object
    [ "status"
        Aeson..= Aeson.object
          [ "code" Aeson..= HttpTypes.statusCode (Response.status response)
          , "message" Aeson..= bsToText (HttpTypes.statusMessage (Response.status response))
          ]
    , "headers" Aeson..= formatHeaders (filterHeaders includedHeaders (Response.responseHeaders response))
    , "respondedAt" Aeson..= Response.respondedAt response
    , "responseTime" Aeson..= formatTimeSpec timeUnit (Response.responseTime response)
    ]


-- * Options


data Options m = Options
  { logRequest :: forall a. Request -> m a -> m a
  , logResponse :: forall a. Response -> m a -> m a
  }


options :: Katip.KatipContext m => Formatter Request -> Formatter Response -> Katip.Severity -> Options m
options requestFormatter responseFormatter severity =
  Options
    { logRequest = \request action ->
        Katip.katipAddContext (Katip.sl "request" (requestFormatter request)) $ do
          Katip.logLocM severity "Request received"
          action
    , logResponse = \response action ->
        Katip.katipAddContext (Katip.sl "response" (responseFormatter response)) $ do
          Katip.logLocM severity "Response sent"
          action
    }


defaultOptions :: Katip.KatipContext m => Katip.Severity -> Options m
defaultOptions =
  options
    (defaultRequestFormat defaultIncludeHeaders)
    (defaultResponseFormat defaultIncludeHeaders Seconds)
