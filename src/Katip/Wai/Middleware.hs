module Katip.Wai.Middleware
  ( ApplicationT
  , MiddlewareT
  , Options (..)
  , runApplication
  , middleware
  ) where

import Katip.Wai.Options (Options)
import qualified Katip.Wai.Options as Options
import qualified Katip.Wai.Request as Request
import qualified Katip.Wai.Response as Response

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Katip
import qualified Network.Wai as Wai


-- | Just like 'Wai.Application' except it runs in @m@ instead of 'IO'
type ApplicationT m = Wai.Request -> (Wai.Response -> m Wai.ResponseReceived) -> m Wai.ResponseReceived


-- | Just like 'Wai.Middleware' except it runs in @m@ instead of 'IO'
type MiddlewareT m = ApplicationT m -> ApplicationT m


-- | Converts an 'ApplicationT' to a normal 'Wai.Application'
runApplication :: MonadIO m => (forall a. m a -> IO a) -> ApplicationT m -> Wai.Application
runApplication toIO application request send =
  toIO $ application request (liftIO . send)


middleware
  :: Katip.KatipContext m
  => Options m
  -> MiddlewareT m
middleware options application request send = do
  tracedRequest <- Request.traceRequest request
  Options.logRequest options tracedRequest $
    application request $ \response -> do
      tracedResponse <- Response.traceResponse tracedRequest response
      Options.logResponse options tracedResponse $
        send response
