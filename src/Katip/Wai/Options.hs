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
  = Microseconds
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
formatTimeSpec =
  error "TODO"


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
    , "headers" Aeson..= formatHeaders (fmap (filterHeaders includedHeaders) Request.requestHeaders request)
    , "isSecure" Aeson..= Request.isSecure request
    , "remoteHost" Aeson..= show (Request.remoteHost request)
    , "pathInfo" Aeson..= Request.pathInfo request
    , "queryString" Aeson..= fmap (bimap bsToText (fmap bsToText)) (Request.queryString request)
    , "receivedAt" Aeson..= Request.receivedAt request
    ]


defaultResponseFormat :: IncludedHeaders -> TimeUnit -> Formatter Response
defaultResponseFormat =
  error "TODO"


-- * Options


data Options m = Options
  { logRequest :: forall a. Request -> m a -> m a
  , logResponse :: forall a. Response -> m a -> m a
  }


options :: Katip.KatipContext m => Formatter Request -> Formatter Response -> Options m
options =
  error "TODO"


defaultOptions :: Katip.KatipContext m => Options m
defaultOptions =
  options
    (defaultRequestFormat defaultIncludeHeaders)
    (defaultResponseFormat defaultIncludeHeaders Seconds)
