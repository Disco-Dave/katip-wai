{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Exception (bracket)
import Control.Monad.IO.Unlift (MonadUnliftIO (withRunInIO))
import Data.Proxy (Proxy (Proxy))
import qualified Katip
import Katip.Wai (ApplicationT, runApplication)
import qualified Katip.Wai
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Servant
import System.IO (stdout)

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
            toHandler = Katip.runKatipContextT logEnv context namespace
            hoistedServer = Servant.hoistServer proxy toHandler server
         in Servant.serve proxy hoistedServer

  withRunInIO $ \toIO -> hoistedApp request (toIO . send)

withLogEnv :: (Katip.LogEnv -> IO a) -> IO a
withLogEnv useLogEnv = do
  handleScribe <-
    Katip.mkHandleScribeWithFormatter
      Katip.bracketFormat
      (Katip.ColorLog True)
      stdout
      (Katip.permitItem minBound)
      Katip.V3

  let makeLogEnv =
        Katip.initLogEnv "example-app" "local-dev"
          >>= Katip.registerScribe "stdout" handleScribe Katip.defaultScribeSettings

  bracket makeLogEnv Katip.closeScribes useLogEnv

main :: IO ()
main = withLogEnv $ \logEnv ->
  let toIO = Katip.runKatipContextT logEnv () "main"
      app = runApplication toIO mkApplication
   in Warp.run 5555 app
