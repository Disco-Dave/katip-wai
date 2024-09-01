module Katip.Wai.Example.Middleware
  ( unliftApplication
  , middleware
  ) where

import Control.Exception (SomeException)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (local))
import Katip qualified
import Katip.Wai qualified
import Katip.Wai.Example.AppData qualified as AppData
import Katip.Wai.Example.AppM (AppM)
import Network.Wai qualified as Wai
import UnliftIO (askRunInIO)
import UnliftIO.Exception qualified


unliftApplication :: Katip.Wai.ApplicationT AppM -> AppM Wai.Application
unliftApplication application = do
  runInIO <- askRunInIO
  pure $ \request send ->
    runInIO $ application request (liftIO . send)


logRequestAndResponse :: Katip.Wai.MiddlewareT AppM
logRequestAndResponse =
  Katip.Wai.middlewareCustom $
    let
      katipWaiOptions = Katip.Wai.defaultOptions @AppM Katip.InfoS
     in
      katipWaiOptions
        { Katip.Wai.logRequest = \request action ->
            Katip.Wai.logRequest katipWaiOptions request $
              local
                ( \appData ->
                    appData{AppData.activeRequest = Just request}
                )
                action
        }


logUnhandledException :: Katip.Wai.MiddlewareT AppM
logUnhandledException application request respond =
  application request respond `UnliftIO.Exception.withException` \(e :: SomeException) ->
    Katip.logLocM Katip.ErrorS (Katip.showLS e)


middleware :: Katip.Wai.MiddlewareT AppM
middleware =
  logRequestAndResponse . logUnhandledException
