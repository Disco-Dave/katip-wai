module Katip.Wai.Example.AppM
  ( AppM (..)
  , toIO
  , toHandler
  ) where

import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Except (ExceptT (..), MonadError (..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader (local), ReaderT (runReaderT), asks)
import Data.Coerce (coerce)
import Katip qualified
import Katip.Monadic qualified
import Katip.Wai.Example.AppData (AppData)
import Katip.Wai.Example.AppData qualified as AppData
import Servant qualified
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception qualified


newtype AppM a = AppM (ReaderT AppData IO a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadUnliftIO
    , MonadFail
    , MonadThrow
    , MonadCatch
    , MonadReader AppData
    )


toIO :: AppData -> AppM a -> IO a
toIO appData (AppM action) =
  runReaderT action appData


toHandler :: AppData -> AppM a -> Servant.Handler a
toHandler appData action =
  coerce $ toIO appData (UnliftIO.Exception.try @_ @Servant.ServerError action)


instance MonadError Servant.ServerError AppM where
  throwError = UnliftIO.Exception.throwIO
  catchError = UnliftIO.Exception.catch


instance Katip.Katip AppM where
  getLogEnv :: AppM Katip.LogEnv
  getLogEnv =
    asks (.katip.ltsLogEnv)


  localLogEnv :: (Katip.LogEnv -> Katip.LogEnv) -> AppM a -> AppM a
  localLogEnv f =
    local $ \appData ->
      let katip = appData.katip
       in appData
            { AppData.katip =
                katip
                  { Katip.Monadic.ltsLogEnv = f katip.ltsLogEnv
                  }
            }


instance Katip.KatipContext AppM where
  getKatipContext =
    asks (.katip.ltsContext)


  localKatipContext f =
    local $ \appData ->
      let katip = appData.katip
       in appData
            { AppData.katip =
                katip
                  { Katip.Monadic.ltsContext = f katip.ltsContext
                  }
            }


  getKatipNamespace =
    asks (.katip.ltsNamespace)


  localKatipNamespace f =
    local $ \appData ->
      let katip = appData.katip
       in appData
            { AppData.katip =
                katip
                  { Katip.Monadic.ltsNamespace = f katip.ltsNamespace
                  }
            }
