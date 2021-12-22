{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module LogEntry (
  Headers (..),
  Request (..),
  Response (..),
  LogData (..),
  LogEntry (..),
  toLogEntry,
  isMostlySameAs,
) where

import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import Data.Function (on)
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import qualified Katip

data Headers = Headers
  { host :: Maybe Text
  , referer :: Maybe Text
  , userAgent :: Maybe Text
  , range :: Maybe Text
  }
  deriving (Show, Generic, Eq)

instance Aeson.FromJSON Headers

data Request = Request
  { id :: UUID
  , httpVersion :: Text
  , remoteHost :: Text
  , isSecure :: Bool
  , method :: Text
  , path :: Text
  , queryString :: [(Text, Maybe Text)]
  , bodyLength :: Text
  , headers :: Headers
  }
  deriving (Show, Generic)

instance Aeson.FromJSON Request

data Response = Response
  { elapsedTimeInNanoSeconds :: Natural
  , status :: Int
  }
  deriving (Show, Generic)

instance Aeson.FromJSON Response

data LogData = LogData
  { request :: Maybe Request
  , response :: Maybe Response
  }
  deriving (Generic, Show)

instance Aeson.FromJSON LogData

data LogEntry = LogEntry
  { logMessage :: Text
  , logData :: LogData
  , logSeverity :: Katip.Severity
  }
  deriving (Show)

instance Aeson.FromJSON LogEntry where
  parseJSON = Aeson.withObject "LogEntry" $ \obj ->
    LogEntry <$> (obj .: "msg") <*> (obj .: "data") <*> (obj .: "sev")

toLogEntry :: Aeson.Value -> IO LogEntry
toLogEntry value =
  case Aeson.fromJSON value of
    Aeson.Success entry -> pure entry
    Aeson.Error reason -> fail reason

isMostlySameAs :: LogEntry -> LogEntry -> Bool
isMostlySameAs log1 log2 =
  let compareResponse resp1 resp2 =
        on (==) (fmap status) resp1 resp2
      compareHeaders head1 head2 =
        on (==) (fmap referer) head1 head2
          && on (==) (fmap userAgent) head1 head2
          && on (==) (fmap range) head1 head2
      compareRequest req1 req2 =
        on (==) (fmap httpVersion) req1 req2
          && on (==) (fmap isSecure) req1 req2
          && on (==) (fmap method) req1 req2
          && on (==) (fmap path) req1 req2
          && on (==) (fmap queryString) req1 req2
          && on (==) (fmap bodyLength) req1 req2
          && on compareHeaders (fmap headers) req1 req2
   in on (==) logMessage log1 log2
        && on compareRequest (request . logData) log1 log2
        && on compareResponse (response . logData) log1 log2

instance Eq LogEntry where
  (==) = isMostlySameAs
