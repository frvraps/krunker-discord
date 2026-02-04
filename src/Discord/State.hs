module Discord.State
  ( BotState (..),
    newBotState,
    storeSessionInfo,
    clearSessionState,
  )
where

import Control.Concurrent.STM (TVar, atomically, newTVarIO, writeTVar)
import Data.Text (Text)

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

storeSessionInfo :: BotState -> Text -> Text -> IO ()
storeSessionInfo state sessId resumeUrl = atomically $ do
  writeTVar (stateSessionId state) (Just sessId)
  writeTVar (stateResumeUrl state) (Just resumeUrl)

clearSessionState :: BotState -> IO ()
clearSessionState state = atomically $ do
  writeTVar (stateSessionId state) Nothing
  writeTVar (stateResumeUrl state) Nothing
