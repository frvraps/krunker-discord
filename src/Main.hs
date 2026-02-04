{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO)
import qualified Data.Text as T
import Discord.Client (Client, newClient)
import qualified Discord.Endpoints.Channel as Channel
import Discord.Gateway (EventHandler, connectAndRun)
import Discord.State (newBotState)
import Discord.Types (Author (..), Event (..))
import System.Environment (getEnv)

main :: IO ()
main = do
  discordToken <- T.pack <$> getEnv "DISCORD_TOKEN"
  client <- newClient discordToken
  state <- newBotState
  connectAndRun discordToken state (handleEvent client)

handleEvent :: Client -> EventHandler
handleEvent client event = do
  _ <- forkIO $ case event of
    ReadyEvent {} -> putStrLn "Ready!"
    MessageCreateEvent {msgContent = content, msgChannelId = channelId, msgAuthor = author}
      | authorBot author /= Just True -> do
          _ <- Channel.sendMessage client channelId ("You said: " <> content)
          pure ()
      | otherwise -> pure ()
    UnknownEvent name _ -> putStrLn $ "Unknown event: " <> T.unpack name
  pure ()
