module Katip.Wai.Example.Servant
  ( application
  ) where

import Control.Monad.Reader (MonadReader (..))
import Data.Proxy (Proxy (Proxy))
import Katip qualified
import Katip.Wai.Example.AppM (AppM)
import Katip.Wai.Example.AppM qualified as AppM
import Katip.Wai.Example.Middleware qualified as Middleware
import Network.Wai qualified as Wai
import Servant qualified
import UnliftIO (MonadUnliftIO (..))


-- * GET


type Get = Servant.GetNoContent


get :: AppM Servant.NoContent
get = do
  Katip.logLocM Katip.DebugS "Notice that the request information is in the context of this log message."
  pure Servant.NoContent


-- * Application


type Api =
  Get


server :: Servant.ServerT Api AppM
server =
  get


application :: AppM Wai.Application
application = do
  Middleware.unliftApplication $
    Middleware.middleware $ \request send -> do
      appData <- ask
      let app =
            let api = Proxy @Api
                hoistedServer = Servant.hoistServer api (AppM.toHandler appData) server
             in Servant.serve api hoistedServer

      withRunInIO $ \runInIO -> app request (runInIO . send)
