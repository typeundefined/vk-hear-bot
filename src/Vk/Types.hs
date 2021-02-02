{-# LANGUAGE OverloadedStrings #-}
module Vk.Types
  ( Handle(..)
  , createHandle
  , LongPollServerInfo(..)
  , ErrorInfo(..)
  , Updates(..)
  , Event(..)
  ) where

import Data.Aeson

data Handle = Handle
  { _key            :: String
  , _serverInfo     :: Maybe LongPollServerInfo
  , _lastReadTs     :: Int
  , _lastReadUpdate :: Updates
  , _comment        :: String
  }
  deriving Show

createHandle apiKey = Handle
  { _key            = apiKey
  , _serverInfo     = Nothing
  , _lastReadTs     = 0
  , _lastReadUpdate = Empty
  , _comment        = ""
  }

data LongPollServerInfo = LongPollServerInfo
  { _lpServer :: String
  , _lpTs     :: Int
  , _lpKey    :: String
  }
  deriving Show

instance FromJSON LongPollServerInfo where
  parseJSON = withObject "longPollServerInfo" $ \o -> do
    resp <- o .: "response"
    srv  <- resp .: "server"
    ts   <- resp .: "ts"
    key  <- resp .: "key"
    return $ LongPollServerInfo { _lpServer = srv, _lpKey = key, _lpTs = ts }

data Updates = Unsupported
             | Empty
             | Message {userId :: Int, messageText :: String}
             deriving (Show)

data Event = Event
  { _updates :: Updates
  , _ts      :: Int
  , _pts     :: Double
  }
  deriving Show

data ErrorInfo = ErrorInfo
  { errorCode :: Int
  , errorMsg  :: String
  }
  deriving Show

instance FromJSON ErrorInfo where
  parseJSON = withObject "errorInfo" $ \o -> do
    errObj <- o .: "error"
    code   <- errObj .: "error_code"
    msg    <- errObj .: "error_msg"
    return $ ErrorInfo code msg
