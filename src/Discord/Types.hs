{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Discord.Types
  ( Incoming (..),
    Outgoing (..),
    IdentifyData (..),
    IdentifyProperties (..),
    HelloData (..),
    HeartbeatData (..),
    ReadyData (..),
    ResumeData (..),
    GatewayException (..),
    opcodeDispatch,
    opcodeHeartbeat,
    opcodeIdentify,
    opcodeResume,
    opcodeReconnect,
    opcodeInvalidSession,
    opcodeHello,
    opcodeHeartbeatAck,
  )
where

import Control.Exception (Exception)
import Data.Aeson (FromJSON (parseJSON), Value, camelTo2, genericParseJSON, object, (.=))
import Data.Aeson.Types (Options (..), ToJSON (..), defaultOptions)
import Data.Text (Text)
import GHC.Generics (Generic)

data Incoming = Incoming
  { op :: Int,
    d :: Maybe Value,
    s :: Maybe Int,
    t :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON Incoming

data Outgoing a = Outgoing
  { outOp :: Int,
    outD :: a
  }
  deriving (Generic)

instance (ToJSON a) => ToJSON (Outgoing a) where
  toJSON (Outgoing opcode dat) =
    object
      [ "op" .= opcode,
        "d" .= dat
      ]

data IdentifyProperties = IdentifyProperties
  { os :: Text,
    browser :: Text,
    device :: Text
  }
  deriving (Show, Generic)

instance ToJSON IdentifyProperties

data IdentifyData = IdentifyData
  { token :: Text,
    intents :: Int,
    properties :: IdentifyProperties
  }
  deriving (Show, Generic)

instance ToJSON IdentifyData

data HelloData = HelloData
  { heartbeatInterval :: Int
  }
  deriving (Show, Generic)

instance FromJSON HelloData where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = camelTo2 '_'
        }

newtype HeartbeatData = HeartbeatData
  { sequenceNum :: Maybe Int
  }
  deriving (Show, Generic)

instance ToJSON HeartbeatData where
  toJSON (HeartbeatData seqNum) = toJSON seqNum

data ReadyData = ReadyData
  { sessionId :: Text,
    resumeGatewayUrl :: Text
  }
  deriving (Show, Generic)

instance FromJSON ReadyData where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = camelTo2 '_'
        }

data ResumeData = ResumeData
  { resumeToken :: Text,
    resumeSessionId :: Text,
    resumeSeq :: Maybe Int
  }
  deriving (Show, Generic)

instance ToJSON ResumeData where
  toJSON (ResumeData tok sessId seqNum) =
    object
      [ "token" .= tok,
        "session_id" .= sessId,
        "seq" .= seqNum
      ]

data GatewayException = ReconnectRequested | ZombieConnection
  deriving (Show)

instance Exception GatewayException

opcodeDispatch :: Int
opcodeDispatch = 0

opcodeHeartbeat :: Int
opcodeHeartbeat = 1

opcodeIdentify :: Int
opcodeIdentify = 2

opcodeResume :: Int
opcodeResume = 6

opcodeReconnect :: Int
opcodeReconnect = 7

opcodeInvalidSession :: Int
opcodeInvalidSession = 9

opcodeHello :: Int
opcodeHello = 10

opcodeHeartbeatAck :: Int
opcodeHeartbeatAck = 11
