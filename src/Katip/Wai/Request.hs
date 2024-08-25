module Katip.Wai.Request
  ( Request (..)
  , traceRequest
  ) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Time as Time
import Data.UUID (UUID)
import qualified Data.UUID.V4 as V4
import qualified Network.HTTP.Types as HttpTypes
import qualified Network.Socket as Socket
import qualified Network.Wai as Wai
import qualified System.Clock as Clock


data Request = Request
  { traceId :: UUID
  , method :: HttpTypes.Method
  , httpVersion :: HttpTypes.HttpVersion
  , rawPathInfo :: ByteString
  , requestHeaders :: HttpTypes.RequestHeaders
  , isSecure :: Bool
  , remoteHost :: Socket.SockAddr
  , pathInfo :: [Text]
  , queryString :: HttpTypes.Query
  , receivedAt :: Time.UTCTime
  , startedAt :: Clock.TimeSpec
  }
  deriving (Show, Eq)


traceRequest :: MonadIO m => Wai.Request -> m Request
traceRequest waiRequest = do
  traceId <- liftIO V4.nextRandom
  receivedAt <- liftIO Time.getCurrentTime
  startedAt <- liftIO $ Clock.getTime Clock.Monotonic

  let request =
        Request
          { traceId = traceId
          , receivedAt = receivedAt
          , startedAt = startedAt
          , method = Wai.requestMethod waiRequest
          , httpVersion = Wai.httpVersion waiRequest
          , rawPathInfo = Wai.rawPathInfo waiRequest
          , requestHeaders = Wai.requestHeaders waiRequest
          , isSecure = Wai.isSecure waiRequest
          , remoteHost = Wai.remoteHost waiRequest
          , pathInfo = Wai.pathInfo waiRequest
          , queryString = Wai.queryString waiRequest
          }

  pure request
