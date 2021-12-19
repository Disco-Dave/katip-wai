{-# LANGUAGE OverloadedStrings #-}

module Katip.WaiSpec (spec) where

import Data.Foldable (for_)
import qualified Data.Text as Text
import DebugApplication (withDebugApplication)
import qualified DebugApplication
import qualified Katip
import qualified Network.HTTP.Client as Http
import Test.Hspec (Spec, describe, it, runIO, shouldNotBe)

allSeverities :: [Katip.Severity]
allSeverities = [minBound ..]

spec :: Spec
spec = describe "middleware" $ do
  manager <- runIO $ Http.newManager Http.defaultManagerSettings

  for_ allSeverities $ \severity -> do
    let prettySeverity = Text.unpack . Text.toLower $ Katip.renderSeverity severity

    it ("logs as " <> prettySeverity <> " and adds the request information to the context") $ do
      logs <- withDebugApplication severity $ \debugApp -> do
        let message = "Context should be added when severity is " <> Text.pack prettySeverity
         in DebugApplication.sendLogRequest manager debugApp "GET" message

      logs `shouldNotBe` []

    it ("logs as " <> prettySeverity <> " and uses a different requestId for every request") $ do
      logs <- withDebugApplication severity $ \debugApp ->
        for_ ["GET", "POST", "PATCH", "PUT", "DELETE"] $ \method -> do
          DebugApplication.sendNoContentRequest manager debugApp method
          DebugApplication.sendNotFoundRequest manager debugApp method
          DebugApplication.sendLogRequest manager debugApp method "Some example log message"

      logs `shouldNotBe` []
