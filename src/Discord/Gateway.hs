{-# LANGUAGE OverloadedStrings #-}

module Discord.Gateway
  ( connectAndRun,
    EventHandler,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM (atomically, readTVar, readTVarIO, writeTVar)
import Control.Exception (SomeException, catch, throwIO)
import Control.Monad (forever, guard, unless)
import Data.Aeson (Result (..), Value, eitherDecode, encode, fromJSON)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Discord.State (BotState (..), clearSessionState, storeSessionInfo)
import Discord.Types
import Network.WebSockets (ClientApp, Connection, receiveData)
import Network.WebSockets.Connection (sendTextData)
import Wuss (runSecureClient)

type EventHandler = Text -> Value -> IO ()

connectAndRun :: Text -> BotState -> EventHandler -> IO ()
connectAndRun dToken state handler = go Nothing
  where
    go maybeResumeUrl = do
      let (host, path) =
            maybe
              ("gateway.discord.gg", "/?v=10&encoding=json")
              parseResumeUrl
              maybeResumeUrl
      putStrLn $ "Connecting to " <> host
      catch
        (runSecureClient host 443 path $ ws dToken state handler (isJust maybeResumeUrl))
        (handleDisconnect go)

    handleDisconnect reconnect e = do
      putStrLn $ "Disconnected: " <> show (e :: SomeException)
      retryCount <- readTVarIO $ stateRetryCount state
      let delay = min 60 (2 ^ retryCount) * 1000000
      putStrLn $ "Waiting " <> show (delay `div` 1000000) <> "s before reconnecting..."
      atomically $ writeTVar (stateRetryCount state) (retryCount + 1)
      threadDelay delay
      resumeUrl <- readTVarIO $ stateResumeUrl state
      putStrLn "Attempting reconnection..."
      reconnect resumeUrl

ws :: Text -> BotState -> EventHandler -> Bool -> ClientApp ()
ws dToken state handler shouldResume connection = do
  putStrLn "Connected"
  atomically $ do
    writeTVar (stateRetryCount state) 0
    writeTVar (stateHeartbeatAck state) True
  helloData <- expectHello connection
  let interval = heartbeatInterval helloData
  putStrLn $ "Heartbeat interval: " <> show interval
  authenticate connection dToken state shouldResume
  withAsync (heartbeatLoop connection state interval) $ \_ ->
    eventLoop connection state handler

expectHello :: Connection -> IO HelloData
expectHello conn = do
  hello <- receiveIncoming conn
  case hello of
    Left err -> fail $ "Failed to decode message: " <> err
    Right incoming ->
      maybe (fail "Expected HELLO") pure (getHelloData incoming)

authenticate :: Connection -> Text -> BotState -> Bool -> IO ()
authenticate conn dToken state shouldResume
  | shouldResume = putStrLn "Sending RESUME" >> sendResume conn dToken state
  | otherwise = putStrLn "Sending IDENTIFY" >> sendIdentify conn dToken

heartbeatLoop :: Connection -> BotState -> Int -> IO ()
heartbeatLoop conn state interval = forever $ do
  threadDelay (interval * 1000)
  sendHeartbeat conn state

eventLoop :: Connection -> BotState -> EventHandler -> IO ()
eventLoop conn state handler = forever $ do
  event <- receiveIncoming conn
  case event of
    Left err -> putStrLn $ "Failed to decode message: " <> err
    Right incoming -> do
      updateSeqNum state incoming
      handleGatewayEvent conn state incoming
      handleDispatchEvent handler incoming

updateSeqNum :: BotState -> Incoming -> IO ()
updateSeqNum state incoming =
  case s incoming of
    Just seqNum -> atomically $ writeTVar (stateSeqNum state) (Just seqNum)
    Nothing -> pure ()

handleGatewayEvent :: Connection -> BotState -> Incoming -> IO ()
handleGatewayEvent conn state incoming
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

handleDispatchEvent :: EventHandler -> Incoming -> IO ()
handleDispatchEvent handler incoming
  | op incoming == opcodeDispatch,
    Just eventName <- t incoming,
    Just eventData <- d incoming =
      handler eventName eventData
  | otherwise = pure ()

receiveIncoming :: Connection -> IO (Either String Incoming)
receiveIncoming conn = eitherDecode <$> receiveData conn

sendIdentify :: Connection -> Text -> IO ()
sendIdentify conn dToken =
  sendTextData conn $
    encode $
      Outgoing opcodeIdentify $
        IdentifyData
          { token = dToken,
            intents = 513,
            properties =
              IdentifyProperties
                { os = "linux",
                  browser = "krunker-discord",
                  device = "krunker-discord"
                }
          }

sendHeartbeat :: Connection -> BotState -> IO ()
sendHeartbeat conn state = do
  ackReceived <- atomically $ do
    ack <- readTVar (stateHeartbeatAck state)
    writeTVar (stateHeartbeatAck state) False
    pure ack
  unless ackReceived $ throwIO ZombieConnection
  seqNum <- readTVarIO $ stateSeqNum state
  sendTextData conn $ encode $ Outgoing opcodeHeartbeat (HeartbeatData seqNum)

sendResume :: Connection -> Text -> BotState -> IO ()
sendResume conn dToken state = do
  sessId <- readTVarIO $ stateSessionId state
  seqNum <- readTVarIO $ stateSeqNum state
  case sessId of
    Nothing -> putStrLn "No session ID to resume"
    Just sid ->
      sendTextData conn $ encode $ Outgoing opcodeResume (ResumeData dToken sid seqNum)

getHelloData :: Incoming -> Maybe HelloData
getHelloData incoming = do
  guard (op incoming == opcodeHello)
  val <- d incoming
  case fromJSON val of
    Success h -> Just h
    Error _ -> Nothing

getReadyData :: Incoming -> Maybe ReadyData
getReadyData incoming = do
  guard (op incoming == opcodeDispatch)
  guard (t incoming == Just "READY")
  val <- d incoming
  case fromJSON val of
    Success r -> Just r
    Error _ -> Nothing

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
