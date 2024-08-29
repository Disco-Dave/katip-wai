module Katip.Wai.Example.Servant
  ( application
  ) where

import Control.Monad.Reader (MonadReader (..), asks)
import Data.OpenApi (OpenApi)
import Data.Proxy (Proxy (Proxy))
import Data.UUID (UUID)
import Katip qualified
import Katip.Wai qualified
import Katip.Wai.Example.AppData qualified as AppData
import Katip.Wai.Example.AppM (AppM)
import Katip.Wai.Example.AppM qualified as AppM
import Katip.Wai.Example.Middleware qualified as Middleware
import Network.Wai qualified as Wai
import Servant ((:<|>) (..), (:>))
import Servant qualified
import Servant.OpenApi qualified
import Servant.Swagger.UI qualified
import UnliftIO (MonadUnliftIO (..))


-- * GET


type Get = Servant.GetNoContent


get :: AppM Servant.NoContent
get = do
  Katip.logLocM Katip.DebugS "Notice that the request information is in the context of this log message."
  pure Servant.NoContent


-- * :request GET


type GetRequest = "request" :> Servant.Get '[Servant.JSON] (Maybe UUID)


getRequest :: AppM (Maybe UUID)
getRequest = do
  activeRequest <- asks (.activeRequest)
  Katip.logLocM Katip.DebugS "We can also grab the active request info with trace id if setup appropriately"
  pure $ fmap (.traceId) activeRequest


-- * Application


type Api =
  Get
    :<|> GetRequest


server :: Servant.ServerT Api AppM
server =
  get
    :<|> getRequest


type ApiWithSwagger =
  Servant.Swagger.UI.SwaggerSchemaUI "docs" "openapi.json" :<|> Api


openapi :: OpenApi
openapi =
  Servant.OpenApi.toOpenApi @Api Proxy


serverWithSwagger :: Servant.ServerT ApiWithSwagger AppM
serverWithSwagger =
  Servant.Swagger.UI.swaggerSchemaUIServerT openapi
    :<|> server


application :: AppM Wai.Application
application = do
  Middleware.unliftApplication $
    Middleware.middleware $ \request send -> do
      appData <- ask
      let app =
            let api = Proxy @ApiWithSwagger
                hoistedServer = Servant.hoistServer api (AppM.toHandler appData) serverWithSwagger
             in Servant.serve api hoistedServer

      withRunInIO $ \runInIO -> app request (runInIO . send)
