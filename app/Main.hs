{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Exception (Exception, SomeException, catch, throwIO)
import Control.Monad (forever, guard, unless, void)
import Data.Aeson (FromJSON (parseJSON), Result (..), Value, camelTo2, eitherDecode, encode, fromJSON, genericParseJSON, object, (.=))
import Data.Aeson.Types (Options (..), ToJSON (..), defaultOptions)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.WebSockets (ClientApp, Connection, receiveData)
import Network.WebSockets.Connection (sendTextData)
import System.Environment (getEnv)
import Wuss

-- Reference:
-- https://discord.com/developers/docs/events/gateway#connections
main :: IO ()
main = do
  discordToken <- T.pack <$> getEnv "DISCORD_TOKEN"
  state <- newBotState
  connectAndRun discordToken state Nothing

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

data GatewayException = ReconnectRequested | ZombieConnection
  deriving (Show)

instance Exception GatewayException

-- We need to store message sequences as part of discords continuinity/recovery
data BotState = BotState
  { stateSeqNum :: TVar (Maybe Int),
    stateSessionId :: TVar (Maybe Text),
    stateResumeUrl :: TVar (Maybe Text),
    stateHeartbeatAck :: TVar Bool,
    stateRetryCount :: TVar Int
  }

newBotState :: IO BotState
newBotState =
  BotState
    <$> newTVarIO Nothing
    <*> newTVarIO Nothing
    <*> newTVarIO Nothing
    <*> newTVarIO True
    <*> newTVarIO 0

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

-- Parse READY event to get session info for resume
getReadyData :: Incoming -> Maybe ReadyData
getReadyData incoming = do
  guard (op incoming == opcodeDispatch)
  guard (t incoming == Just "READY")
  val <- d incoming
  case fromJSON val of
    Success r -> Just r
    Error _ -> Nothing

-- When sending heartbeats we also update the sequence number in local state
sendHeartbeat :: Connection -> BotState -> IO ()
sendHeartbeat conn state = do
  ackReceived <- atomically $ do
    ack <- readTVar (stateHeartbeatAck state)
    writeTVar (stateHeartbeatAck state) False
    pure ack
  unless ackReceived $ throwIO ZombieConnection
  seqNum <- readTVarIO $ stateSeqNum state
  sendTextData conn $ encode $ Outgoing opcodeHeartbeat (HeartbeatData seqNum)

-- Send resume to reconnect with existing session
sendResume :: Connection -> Text -> BotState -> IO ()
sendResume conn dToken state = do
  sessId <- readTVarIO $ stateSessionId state
  seqNum <- readTVarIO $ stateSeqNum state
  case sessId of
    Nothing -> putStrLn "No session ID to resume"
    Just sid ->
      sendTextData conn $ encode $ Outgoing opcodeResume (ResumeData dToken sid seqNum)

storeSessionInfo :: BotState -> ReadyData -> IO ()
storeSessionInfo state ready = atomically $ do
  writeTVar (stateSessionId state) (Just $ sessionId ready)
  writeTVar (stateResumeUrl state) (Just $ resumeGatewayUrl ready)

isInvalidSession :: Incoming -> Maybe Bool
isInvalidSession incoming = do
  guard (op incoming == opcodeInvalidSession)
  val <- d incoming
  case fromJSON val of
    Success canResume -> Just canResume
    Error _ -> Nothing

parseResumeUrl :: Text -> (String, String)
parseResumeUrl url =
  let withoutProtocol = T.drop 6 url
      host = T.unpack $ T.takeWhile (/= '/') withoutProtocol
   in (host, "/?v=10&encoding=json")

clearSessionState :: BotState -> IO ()
clearSessionState state = atomically $ do
  writeTVar (stateSessionId state) Nothing
  writeTVar (stateResumeUrl state) Nothing

handleEvent :: Connection -> BotState -> Incoming -> IO ()
handleEvent conn state incoming
  | Just ready <- getReadyData incoming = do
      putStrLn $ "Got READY, session: " <> show (sessionId ready)
      storeSessionInfo state ready
  | Just canResume <- isInvalidSession incoming = do
      putStrLn $ "Invalid session, can resume: " <> show canResume
      unless canResume $ clearSessionState state
  | op incoming == opcodeHeartbeatAck =
      atomically $ writeTVar (stateHeartbeatAck state) True
  | op incoming == opcodeReconnect = do
      putStrLn "Reconnect requested by Discord"
      throwIO ReconnectRequested
  | op incoming == opcodeHeartbeat = do
      putStrLn "Heartbeat requested by Discord"
      seqNum <- readTVarIO $ stateSeqNum state
      sendTextData conn $ encode $ Outgoing opcodeHeartbeat (HeartbeatData seqNum)
  | otherwise = pure ()

eventLoop :: Connection -> BotState -> IO ()
eventLoop conn state = forever $ do
  event <- receiveIncoming conn
  case event of
    Left err -> putStrLn $ "Failed to decode message: " <> err
    Right incoming -> do
      case s incoming of
        Just seqNum -> atomically $ writeTVar (stateSeqNum state) (Just seqNum)
        Nothing -> pure ()
      handleEvent conn state incoming
      print incoming

connectAndRun :: Text -> BotState -> Maybe Text -> IO ()
connectAndRun dToken state maybeResumeUrl = do
  let (host, path) =
        maybe
          ("gateway.discord.gg", "/?v=10&encoding=json")
          parseResumeUrl
          maybeResumeUrl
  putStrLn $ "Connecting to " <> host
  catch
    (runSecureClient host 443 path $ ws dToken state (isJust maybeResumeUrl))
    handleDisconnect
  where
    handleDisconnect :: SomeException -> IO ()
    handleDisconnect e = do
      putStrLn $ "Disconnected: " <> show e
      retryCount <- readTVarIO $ stateRetryCount state
      let delay = min 60 (2 ^ retryCount) * 1000000
      putStrLn $ "Waiting " <> show (delay `div` 1000000) <> "s before reconnecting..."
      atomically $ writeTVar (stateRetryCount state) (retryCount + 1)
      threadDelay delay
      resumeUrl <- readTVarIO $ stateResumeUrl state
      putStrLn "Attempting reconnection..."
      connectAndRun dToken state resumeUrl

ws :: Text -> BotState -> Bool -> ClientApp ()
ws dToken state shouldResume connection = do
  putStrLn "Connected"
  atomically $ do
    writeTVar (stateRetryCount state) 0
    writeTVar (stateHeartbeatAck state) True
  hello <- receiveIncoming connection
  case hello of
    Left err -> putStrLn $ "Failed to decode message: " <> err
    Right incoming ->
      case getHelloData incoming of
        Nothing -> putStrLn "Expected HELLO"
        Just helloData -> do
          let interval = heartbeatInterval helloData
          putStrLn $ "Heartbeat interval: " <> show interval
          if shouldResume
            then do
              putStrLn "Sending RESUME"
              sendResume connection dToken state
            else do
              putStrLn "Sending IDENTIFY"
              sendIdentify connection dToken
          void $ forkIO $ forever $ do
            threadDelay (interval * 1000)
            sendHeartbeat connection state
          eventLoop connection state
