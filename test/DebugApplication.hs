{-# LANGUAGE OverloadedStrings #-}

module DebugApplication (
  withDebugApplication,
) where

import Control.Concurrent.Async (Async)
import Control.Exception (bracket)
import qualified Data.Aeson as Aeson
import DebugScribe (withDebugScribe)
import Katip (KatipContextT, runKatipContextT)
import qualified Katip
import Katip.Wai (ApplicationT, runApplication)
import qualified Katip.Wai
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

mkApplication :: Katip.Severity -> ApplicationT (KatipContextT IO)
mkApplication severity =
  let base request send =
        send $ case Wai.pathInfo request of
          [] -> Wai.responseLBS (toEnum 204) [] mempty
          _ -> Wai.responseLBS (toEnum 404) [] mempty
   in Katip.Wai.middleware severity base

withLogEnv :: (Async [Aeson.Value] -> IO a) -> (Katip.LogEnv -> IO ()) -> IO a
withLogEnv useLogs useLogEnv =
  withDebugScribe (Katip.permitItem minBound) Katip.V3 $ \scribe logs -> do
    let makeLogEnv =
          Katip.initLogEnv "debug-app" "local-test"
            >>= Katip.registerScribe "debug" scribe Katip.defaultScribeSettings
     in bracket makeLogEnv Katip.closeScribes useLogEnv *> useLogs logs

withDebugApplication :: Katip.Severity -> (Warp.Port -> IO ()) -> (Async [Aeson.Value] -> IO a) -> IO a
withDebugApplication severity usePort useLogs =
  withLogEnv useLogs $ \logEnv ->
    let toIO = runKatipContextT logEnv () "spec"
        app = runApplication toIO (mkApplication severity)
     in Warp.testWithApplication (pure app) usePort
