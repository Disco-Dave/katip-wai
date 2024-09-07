module Katip.Wai.Example.Short (main) where

import Control.Exception (bracket)
import Data.Proxy (Proxy (Proxy))
import Katip qualified
import Katip.Wai (ApplicationT, runApplication)
import Katip.Wai qualified
import Network.Wai.Handler.Warp qualified as Warp
import Servant qualified
import System.IO (stdout)
import UnliftIO (MonadUnliftIO (withRunInIO))


type Api = Servant.GetNoContent


server :: Servant.ServerT Api (Katip.KatipContextT Servant.Handler)
server = do
  Katip.logLocM Katip.InfoS "This message should also have the request context"
  pure Servant.NoContent


mkApplication :: ApplicationT (Katip.KatipContextT IO)
mkApplication = Katip.Wai.middleware Katip.InfoS $ \request send -> do
  logEnv <- Katip.getLogEnv
  context <- Katip.getKatipContext
  namespace <- Katip.getKatipNamespace

  let hoistedApp =
        let proxy = Proxy @Api
            hoistedServer = Servant.hoistServer proxy (Katip.runKatipContextT logEnv context namespace) server
         in Servant.serve proxy hoistedServer

  withRunInIO $ \toIO -> hoistedApp request (toIO . send)


withLogEnv :: (Katip.LogEnv -> IO a) -> IO a
withLogEnv useLogEnv = do
  handleScribe <-
    Katip.mkHandleScribeWithFormatter
      Katip.jsonFormat
      (Katip.ColorLog False)
      stdout
      (Katip.permitItem minBound)
      Katip.V3

  let makeLogEnv =
        Katip.initLogEnv "example-app" "local-dev"
          >>= Katip.registerScribe "stdout" handleScribe Katip.defaultScribeSettings

  bracket makeLogEnv Katip.closeScribes useLogEnv


main :: IO ()
main = withLogEnv $ \logEnv ->
  let
    app = runApplication (Katip.runKatipContextT logEnv () "main") mkApplication
   in
    Warp.run 5555 app
