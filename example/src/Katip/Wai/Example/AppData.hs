module Katip.Wai.Example.AppData
  ( AppData (..)
  , withAppData
  ) where

import Control.Monad.Catch (bracket)
import Katip
import Katip.Monadic qualified
import Katip.Wai qualified
import System.IO (stdout)


data AppData = AppData
  { activeRequest :: Maybe Katip.Wai.Request
  , katip :: Katip.Monadic.KatipContextTState
  }


withAppData :: (AppData -> IO a) -> IO a
withAppData use = do
  let
    makeLogEnv = do
      env <- Katip.initLogEnv "katip-wai-example" "local_development"

      handleScribe <-
        Katip.mkHandleScribeWithFormatter
          Katip.jsonFormat
          (Katip.ColorLog False)
          stdout
          (Katip.permitItem minBound)
          maxBound

      Katip.registerScribe "stdout" handleScribe Katip.defaultScribeSettings env

  bracket makeLogEnv Katip.closeScribes $ \logEnv ->
    use
      AppData
        { activeRequest = Nothing
        , katip =
            Katip.Monadic.KatipContextTState
              { Katip.Monadic.ltsLogEnv = logEnv
              , Katip.Monadic.ltsContext = mempty
              , Katip.Monadic.ltsNamespace = Katip.Namespace []
              }
        }
