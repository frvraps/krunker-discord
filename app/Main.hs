{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVarIO, writeTVar)
import Control.Monad (forever, guard)
import Data.Aeson (FromJSON (parseJSON), Result (..), Value, camelTo2, eitherDecode, encode, fromJSON, genericParseJSON, object, (.=))
import Data.Aeson.Types (Options (..), ToJSON (..), defaultOptions)
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Network.WebSockets (ClientApp, Connection, receiveData)
import Network.WebSockets.Connection (sendTextData)
import System.Environment (getEnv)
import Wuss

-- Reference:
-- https://discord.com/developers/docs/events/gateway#connections
main :: IO ()
main = do
  discordToken <- pack <$> getEnv "DISCORD_TOKEN"
  runSecureClient "gateway.discord.gg" 443 "/?v=10&encoding=json" $ ws discordToken

-- Reference:
-- https://discord.com/developers/docs/events/gateway-events#payload-structure
-- Types for incoming and outgoing messages
data Incoming = Incoming
  { op :: Int,
    d :: Maybe Value,
    s :: Maybe Int,
    t :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON Incoming

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

-- Op codes used for incoming and outgoing messages
opcodeHello :: Int
opcodeHello = 10

opcodeIdentify :: Int
opcodeIdentify = 2

opcodeHeartbeat :: Int
opcodeHeartbeat = 1

-- We need to store message sequences as part of discords continuinity/recovery
data BotState = BotState
  { stateSeqNum :: TVar (Maybe Int),
    stateSessionId :: TVar (Maybe Text)
  }

newBotState :: IO BotState
newBotState = BotState <$> newTVarIO Nothing <*> newTVarIO Nothing

-- helpers for receiving/sending data
receiveIncoming :: Connection -> IO (Either String Incoming)
receiveIncoming conn = do
  msg <- receiveData conn
  pure (eitherDecode msg)

-- used for initialising and authenticating the bot, token comes from env
sendIdentify :: Connection -> Text -> IO ()
sendIdentify conn dToken =
  sendTextData conn
    $ encode
    $ Outgoing
      opcodeIdentify
    $ IdentifyData
      { token = dToken,
        intents = 513,
        properties =
          IdentifyProperties
            { os = "os",
              browser = "browser",
              device = "device"
            }
      }

-- The 'hello' response from discord that contains heartbeat info
getHelloData :: Incoming -> Maybe HelloData
getHelloData incoming = do
  guard (op incoming == opcodeHello)
  val <- d incoming
  case fromJSON val of
    Success h -> Just h
    Error _ -> Nothing

-- When sending heartbeats we also update the sequence number in local state
sendHeartbeat :: Connection -> BotState -> IO ()
sendHeartbeat conn state = do
  seqNum <- readTVarIO $ stateSeqNum state
  sendTextData conn $ encode $ Outgoing opcodeHeartbeat (HeartbeatData seqNum)

-- The main event loop for recieving actual discord events
eventLoop :: Connection -> BotState -> IO ()
eventLoop conn state = forever $ do
  event <- receiveIncoming conn
  case event of
    Left err -> putStrLn $ "Failed to decode message: " <> err
    Right incoming -> do
      case s incoming of
        Just seqNum -> atomically $ writeTVar (stateSeqNum state) (Just seqNum)
        Nothing -> pure ()
      print incoming

-- Main app entry point
ws :: Text -> ClientApp ()
ws dToken connection = do
  putStrLn "Connected"
  state <- newBotState -- init bot state
  hello <- receiveIncoming connection -- receive the next event, should be a 'hello' event
  case hello of
    Left err -> putStrLn $ "Failed to decode message: " <> err
    Right incoming ->
      case getHelloData incoming of
        Nothing -> putStrLn "Expected HELLO"
        Just helloData -> do
          let interval = heartbeatInterval helloData
          putStrLn $ "Heartbeat interval: " <> show interval
          sendIdentify connection dToken
          -- heartbeat in the background
          _ <- forkIO $ forever $ do
            threadDelay (interval * 1000)
            sendHeartbeat connection state
          -- handle events
          eventLoop connection state
