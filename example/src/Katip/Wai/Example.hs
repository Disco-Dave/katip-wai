module Katip.Wai.Example
  ( servant
  ) where

import Control.Exception qualified
import Data.Function ((&))
import Katip qualified
import Katip.Wai.Example.AppData qualified as AppData
import Katip.Wai.Example.AppM qualified as AppM
import Katip.Wai.Example.Servant qualified as Servant
import Network.Wai.Handler.Warp qualified as Warp
import System.IO (stdout)
import UnliftIO (MonadUnliftIO (withRunInIO))
import UnliftIO.Concurrent qualified
import UnliftIO.Exception qualified


servant :: Warp.Port -> IO ()
servant port = do
  AppData.withAppData $ \appData ->
    AppM.toIO appData $ do
      Katip.logLocM Katip.DebugS "Is this working?"
      application <- Servant.application
      withRunInIO $ \runInIO ->
        let beforeMainLoop = do
              let uri = "http://localhost:" <> Katip.ls (show port)

              runInIO . Katip.logLocM Katip.InfoS $ "Server running at " <> uri

              runInIO . Katip.logLocM Katip.InfoS $
                "Visit Swagger at " <> uri <> "/docs"

            settings =
              Warp.defaultSettings
                & Warp.setBeforeMainLoop beforeMainLoop
                & Warp.setPort port
                & Warp.setOnException (\_ _ -> pure ()) -- omit logging because middleware should have done this already
         in Warp.runSettings settings application
