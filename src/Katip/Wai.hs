{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Katip.Wai (
  ApplicationT,
  runApplication,
  MiddlewareT,
  middleware,
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID.V4
import qualified Katip
import Network.HTTP.Types (Method)
import Network.HTTP.Types.Status (Status)
import Network.HTTP.Types.URI (Query, queryToQueryText)
import Network.HTTP.Types.Version (HttpVersion)
import Network.Socket (SockAddr)
import qualified Network.Wai as Wai
import qualified System.Clock as Clock

data Request = Request
  { requestId :: UUID
  , requestHttpVersion :: HttpVersion
  , requestRemoteHost :: SockAddr
  , requestIsSecure :: Bool
  , requestMethod :: Method
  , requestPathInfo :: [Text]
  , requestQueryString :: Query
  , requestBodyLength :: Wai.RequestBodyLength
  , requestHeaderHost :: Maybe ByteString
  , requestHeaderReferer :: Maybe ByteString
  , requestHeaderUserAgent :: Maybe ByteString
  , requestHeaderRange :: Maybe ByteString
  }

requestToKeyValues :: Aeson.KeyValue kv => Request -> [kv]
requestToKeyValues Request{..} =
  let toText = decodeUtf8With lenientDecode
      headers =
        Aeson.object
          [ "host" .= fmap toText requestHeaderHost
          , "referer" .= fmap toText requestHeaderReferer
          , "userAgent" .= fmap toText requestHeaderUserAgent
          , "range" .= fmap toText requestHeaderRange
          ]
   in [ "id" .= UUID.toText requestId
      , "httpVersion" .= show requestHttpVersion
      , "remoteHost" .= show requestRemoteHost
      , "isSecure" .= requestIsSecure
      , "method" .= toText requestMethod
      , "path" .= requestPathInfo
      , "queryString" .= queryToQueryText requestQueryString
      , "bodyLength" .= show requestBodyLength
      , "headers" .= headers
      ]

instance Aeson.ToJSON Request where
  toJSON = Aeson.object . requestToKeyValues
  toEncoding = Aeson.pairs . mconcat . requestToKeyValues

toLoggableRequest :: Wai.Request -> IO Request
toLoggableRequest request = do
  requestId <- UUID.V4.nextRandom
  pure
    Request
      { requestId = requestId
      , requestHttpVersion = Wai.httpVersion request
      , requestRemoteHost = Wai.remoteHost request
      , requestIsSecure = Wai.isSecure request
      , requestMethod = Wai.requestMethod request
      , requestPathInfo = Wai.pathInfo request
      , requestQueryString = Wai.queryString request
      , requestBodyLength = Wai.requestBodyLength request
      , requestHeaderHost = Wai.requestHeaderHost request
      , requestHeaderReferer = Wai.requestHeaderReferer request
      , requestHeaderUserAgent = Wai.requestHeaderUserAgent request
      , requestHeaderRange = Wai.requestHeaderRange request
      }

data Response = Response
  { responseElapsedTime :: Clock.TimeSpec
  , responseStatus :: Status
  }

responseToKeyValues :: Aeson.KeyValue kv => Response -> [kv]
responseToKeyValues Response{..} =
  [ "elapsedTimeInNanoSeconds" .= Clock.toNanoSecs responseElapsedTime
  , "status" .= fromEnum responseStatus
  ]

instance Aeson.ToJSON Response where
  toJSON = Aeson.object . responseToKeyValues
  toEncoding = Aeson.pairs . mconcat . responseToKeyValues

-- | Just like 'Wai.Application' except it runs in @m@ instead of 'IO'
type ApplicationT m = Wai.Request -> (Wai.Response -> m Wai.ResponseReceived) -> m Wai.ResponseReceived

-- | Converts an 'ApplicationT' to a normal 'Wai.Application'
runApplication :: MonadIO m => (forall a. m a -> IO a) -> ApplicationT m -> Wai.Application
runApplication toIO application request send =
  toIO $ application request (liftIO . send)

-- | Just like 'Wai.Middleware' except it runs in @m@ instead of 'IO'
type MiddlewareT m = ApplicationT m -> ApplicationT m

withLoggedResponse ::
  Katip.KatipContext m =>
  Katip.Severity ->
  Clock.TimeSpec ->
  (Wai.Response -> m Wai.ResponseReceived) ->
  Wai.Response ->
  m Wai.ResponseReceived
withLoggedResponse severity start send response = do
  responseReceived <- send response
  end <- liftIO $ Clock.getTime Clock.Monotonic
  let loggableResponse =
        Response
          { responseElapsedTime = end `Clock.diffTimeSpec` start
          , responseStatus = Wai.responseStatus response
          }
  Katip.katipAddContext (Katip.sl "response" loggableResponse) $ do
    Katip.logFM severity "Response sent"
    pure responseReceived

-- | Logs the request, response, and elapsed time in Katip's context
middleware :: Katip.KatipContext m => Katip.Severity -> MiddlewareT m
middleware severity application request send = do
  start <- liftIO $ Clock.getTime Clock.Monotonic
  loggableRequest <- liftIO $ toLoggableRequest request
  Katip.katipAddContext (Katip.sl "request" loggableRequest) $ do
    Katip.logFM severity "Request received"
    application request (withLoggedResponse severity start send)
