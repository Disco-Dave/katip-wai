module Katip.Wai.Options
  ( -- * Formatting
    Formatter
  , TimeUnit (..)
  , IncludedHeaders
  , defaultIncludedHeaders
  , defaultRequestFormat
  , defaultResponseFormat

    -- * Options
  , Options (..)
  , addRequestAndResponseToContext
  , logRequestAndResponse
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


-- | A formatter is a function that can convert 'a' into json.
type Formatter a = a -> Aeson.Value


-- | Unit of time to use when logging response times.
data TimeUnit
  = Nanoseconds
  | Microseconds
  | Milliseconds
  | Seconds
  deriving (Show, Eq, Ord, Enum, Bounded)


-- | Headers to include in your logs.
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


-- | Default list of headers to include in logs: 'Host', 'Referer', 'User-Agent', and 'Range'.
defaultIncludedHeaders :: IncludedHeaders
defaultIncludedHeaders =
  Set.fromList
    [ "Host"
    , "Referer"
    , "User-Agent"
    , "Range"
    ]


-- | Default formatter for 'Request's.
defaultRequestFormat :: IncludedHeaders -> Formatter Request
defaultRequestFormat includedHeaders request =
  Aeson.object
    [ "id" Aeson..= UUID.toText (Request.traceId request)
    , "method" Aeson..= bsToText (Request.method request)
    , "httpVersion" Aeson..= show (Request.httpVersion request)
    , "path" Aeson..= bsToText (Request.rawPathInfo request)
    , "headers" Aeson..= formatHeaders (filterHeaders includedHeaders (Request.requestHeaders request))
    , "isSecure" Aeson..= Request.isSecure request
    , "remoteHost" Aeson..= show (Request.remoteHost request)
    , "queryString" Aeson..= fmap (bimap bsToText (fmap bsToText)) (Request.queryString request)
    , "receivedAt" Aeson..= Request.receivedAt request
    ]


-- | Default formatter for 'Response's.
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
  { handleRequest :: forall a. Request -> m a -> m a
  , handleResponse :: forall a. Response -> m a -> m a
  }


addRequestAndResponseToContext :: Katip.KatipContext m => Formatter Request -> Formatter Response -> Options m
addRequestAndResponseToContext requestFormatter responseFormatter =
  Options
    { handleRequest = \request action ->
        Katip.katipAddContext (Katip.sl "request" (requestFormatter request)) $ do
          action
    , handleResponse = \response action ->
        Katip.katipAddContext (Katip.sl "response" (responseFormatter response)) $ do
          action
    }


logRequestAndResponse :: Katip.KatipContext m => Katip.Severity -> Options m
logRequestAndResponse severity =
  Options
    { handleRequest = \_ action -> do
        Katip.logFM severity "Request received."
        action
    , handleResponse = \_ action -> do
        Katip.logFM severity "Response sent."
        action
    }


options :: Katip.KatipContext m => Formatter Request -> Formatter Response -> Katip.Severity -> Options m
options requestFormatter responseFormatter severity =
  mconcat
    [ addRequestAndResponseToContext
        requestFormatter
        responseFormatter
    , logRequestAndResponse severity
    ]


defaultOptions :: Katip.KatipContext m => Katip.Severity -> Options m
defaultOptions =
  options
    (defaultRequestFormat defaultIncludedHeaders)
    (defaultResponseFormat defaultIncludedHeaders Milliseconds)


instance Semigroup (Options m) where
  a <> b =
    Options
      { handleRequest = \request action ->
          handleRequest a request $ handleRequest b request action
      , handleResponse = \response action ->
          handleResponse a response $ handleResponse b response action
      }


instance Monoid (Options m) where
  mempty =
    Options
      { handleRequest = const id
      , handleResponse = const id
      }
