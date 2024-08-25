module Katip.Wai.Response
  ( Response (..)
  , traceResponse
  ) where

import Katip.Wai.Request (Request)
import qualified Katip.Wai.Request as Request

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Time as Time
import qualified Network.HTTP.Types as HttpTypes
import qualified Network.Wai as Wai
import qualified System.Clock as Clock


data Response = Response
  { status :: HttpTypes.Status
  , responseHeaders :: HttpTypes.ResponseHeaders
  , respondedAt :: Time.UTCTime
  , responseTime :: Clock.TimeSpec
  }
  deriving (Show, Eq)


traceResponse :: MonadIO m => Request -> Wai.Response -> m Response
traceResponse request response = do
  endedAt <- liftIO $ Clock.getTime Clock.Monotonic
  respondedAt <- liftIO Time.getCurrentTime

  let responseTime = endedAt `Clock.diffTimeSpec` Request.startedAt request

  pure
    Response
      { status = Wai.responseStatus response
      , responseHeaders = Wai.responseHeaders response
      , respondedAt
      , responseTime
      }
