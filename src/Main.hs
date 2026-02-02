{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import Discord.Gateway (EventHandler, connectAndRun)
import Discord.State (newBotState)
import System.Environment (getEnv)

main :: IO ()
main = do
  discordToken <- T.pack <$> getEnv "DISCORD_TOKEN"
  state <- newBotState
  connectAndRun discordToken state handleEvent

handleEvent :: EventHandler
handleEvent eventName _ =
  putStrLn $ "Event: " <> T.unpack eventName
