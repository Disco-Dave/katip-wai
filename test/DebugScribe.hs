{-# LANGUAGE RankNTypes #-}

module DebugScribe (
  withDebugScribe,
) where

import Control.Concurrent.Async (Async)
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM as STM
import qualified Data.Aeson as Aeson
import qualified Data.Maybe as Maybe
import qualified Katip

withDebugScribe :: Katip.PermitFunc -> Katip.Verbosity -> (Katip.Scribe -> Async [Aeson.Value] -> IO a) -> IO a
withDebugScribe permitFunc verbosity useScribe = do
  queue <- STM.newTQueueIO

  let push logItem =
        let event = Right $ Katip.itemJson verbosity logItem
         in STM.atomically $ STM.writeTQueue queue event

  let finalizer = do
        lock <- MVar.newEmptyMVar
        STM.atomically $ STM.writeTQueue queue (Left lock)
        MVar.readMVar lock

  let scribe =
        Katip.Scribe
          { Katip.liPush = push
          , Katip.scribeFinalizer = finalizer
          , Katip.scribePermitItem = permitFunc
          }

  let handleEvents = do
        event <- STM.atomically $ STM.readTQueue queue
        case event of
          Right _ -> handleEvents
          Left lock -> do
            allEvents <- STM.atomically $ STM.flushTQueue queue

            let toLogValue = either (const Nothing) Just
                allLogs = Maybe.mapMaybe toLogValue allEvents
             in allLogs <$ MVar.putMVar lock ()

  Async.withAsync handleEvents $ useScribe scribe
