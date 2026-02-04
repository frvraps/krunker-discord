{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO)
import qualified Data.Text as T
import Discord.Gateway (EventHandler, connectAndRun)
import Discord.State (newBotState)
import Discord.Types (Event (..))
import System.Environment (getEnv)

main :: IO ()
main = do
  discordToken <- T.pack <$> getEnv "DISCORD_TOKEN"
  state <- newBotState
  connectAndRun discordToken state handleEvent

handleEvent :: EventHandler
handleEvent event = do
  _ <- forkIO $ case event of
    ReadyEvent {} -> putStrLn "Ready!"
    MessageCreateEvent {msgContent = content} -> putStrLn $ "Message: " <> T.unpack content
    UnknownEvent name _ -> putStrLn $ "Unknown event: " <> T.unpack name
  pure ()
