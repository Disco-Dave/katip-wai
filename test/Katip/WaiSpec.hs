{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Katip.WaiSpec (spec) where

import Data.Foldable (for_)
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.UUID as Uuid
import DebugApplication (withDebugApplication)
import qualified DebugApplication
import qualified Katip
import LogEntry (LogEntry (LogEntry))
import qualified LogEntry
import qualified Network.HTTP.Client as Http
import Test.Hspec (Spec, describe, it, runIO, shouldBe, shouldSatisfy)

allSeverities :: [Katip.Severity]
allSeverities = [minBound ..]

spec :: Spec
spec = describe "middleware" $ do
  manager <- runIO $ Http.newManager Http.defaultManagerSettings

  for_ allSeverities $ \severity -> do
    let prettySeverity = Text.unpack . Text.toLower $ Katip.renderSeverity severity

    it ("logs as " <> prettySeverity <> " and adds the request information to the context") $ do
      let message = "Context should be added when severity is " <> Text.pack prettySeverity

      logs <- withDebugApplication severity $ \debugApp ->
        DebugApplication.sendLogRequest manager debugApp "GET" message

      [log1, log2, log3] <- traverse LogEntry.toLogEntry logs

      let expectedRequest =
            LogEntry.Request
              { id = Uuid.nil
              , httpVersion = "HTTP/1.1"
              , remoteHost = "127.0.0.1:1234"
              , isSecure = False
              , method = "GET"
              , path = ["log"]
              , queryString = [("message", Just message)]
              , bodyLength = "KnownLength 0"
              , headers =
                  LogEntry.Headers
                    { host = Just "localhost:1234"
                    , referer = Nothing
                    , userAgent = Nothing
                    , range = Nothing
                    }
              }

      let expectedLog1 =
            LogEntry
              { logMessage = "Request received"
              , logData =
                  LogEntry.LogData
                    { response = Nothing
                    , request = Just expectedRequest
                    }
              , logSeverity = severity
              }
          expectedLog2 =
            LogEntry
              { logMessage = message
              , logData =
                  LogEntry.LogData
                    { response = Nothing
                    , request = Just expectedRequest
                    }
              , logSeverity = severity
              }
          expectedLog3 =
            LogEntry
              { logMessage = "Response sent"
              , logData =
                  LogEntry.LogData
                    { response =
                        Just $
                          LogEntry.Response
                            { elapsedTimeInNanoSeconds = 123
                            , status = 202
                            }
                    , LogEntry.request = Just expectedRequest
                    }
              , logSeverity = severity
              }

      log1 `shouldSatisfy` LogEntry.isMostlySameAs expectedLog1
      log2 `shouldSatisfy` LogEntry.isMostlySameAs expectedLog2
      log3 `shouldSatisfy` LogEntry.isMostlySameAs expectedLog3

    it ("logs as " <> prettySeverity <> " and uses a different requestId for every request") $ do
      logs <- withDebugApplication severity $ \debugApp ->
        for_ ["GET", "POST", "PATCH", "PUT", "DELETE"] $ \method -> do
          DebugApplication.sendNoContentRequest manager debugApp method
          DebugApplication.sendNotFoundRequest manager debugApp method
          DebugApplication.sendLogRequest manager debugApp method "Some example log message"

      parsedLogs <- traverse LogEntry.toLogEntry logs

      let requestIds =
            flip Maybe.mapMaybe parsedLogs $ \entry -> do
              _response <- LogEntry.response $ LogEntry.logData entry
              request <- LogEntry.request $ LogEntry.logData entry
              pure $ LogEntry.id request

          originalLength = length requestIds

          uniqueLength = Set.size $ Set.fromList requestIds

      originalLength `shouldBe` 15

      uniqueLength `shouldBe` originalLength

    it ("logs as " <> prettySeverity <> " and has non zero elapsed time") $ do
      logs <- withDebugApplication severity $ \debugApp ->
        for_ ["GET", "POST", "PATCH", "PUT", "DELETE"] $ \method -> do
          DebugApplication.sendNoContentRequest manager debugApp method
          DebugApplication.sendNotFoundRequest manager debugApp method
          DebugApplication.sendLogRequest manager debugApp method "Some example log message"

      parsedLogs <- traverse LogEntry.toLogEntry logs

      let elapsedTimes =
            flip Maybe.mapMaybe parsedLogs $ \entry -> do
              response <- LogEntry.response $ LogEntry.logData entry
              pure $ LogEntry.elapsedTimeInNanoSeconds response

      length elapsedTimes `shouldBe` 15

      elapsedTimes `shouldSatisfy` all (> 0)
