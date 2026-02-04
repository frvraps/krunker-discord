{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Monad (void)
import qualified Data.Text as T
import Discord.Client (Client, newClient)
import qualified Discord.Endpoints.Command as Command
import qualified Discord.Endpoints.Interaction as Interaction
import Discord.Gateway (EventHandler, connectAndRun)
import Discord.Message
import Discord.State (newBotState)
import Discord.Types
import System.Environment (getEnv)

main :: IO ()
main = do
  discordToken <- T.pack <$> getEnv "DISCORD_TOKEN"
  appId <- T.pack <$> getEnv "DISCORD_APP_ID"
  guildId <- T.pack <$> getEnv "DISCORD_GUILD_ID"

  client <- newClient discordToken
  state <- newBotState

  -- Register slash commands (guild specific for now, this needs to change if bot is public)
  putStrLn "Registering slash commands..."
  result <-
    Command.registerGuildCommands
      client
      appId
      guildId
      [ Command.SlashCommand "ping" "Check if the bot is alive" [],
        Command.SlashCommand
          "player"
          "Look up a Krunker player"
          [ Command.CommandOption "name" "Player name" Command.StringOption True
          ]
      ]
  case result of
    Left err -> putStrLn $ "Failed to register commands: " <> show err
    Right () -> putStrLn "Commands registered"

  connectAndRun discordToken state (handleEvent client)

handleEvent :: Client -> EventHandler
handleEvent client event = void $ forkIO $ case event of
  ReadyEvent {} -> putStrLn "Ready!"
  InteractionCreateEvent interaction ->
    case interactionData interaction of
      Just (SlashCommandData name opts) -> do
        putStrLn $ "Slash command: " <> T.unpack name
        handleSlashCommand client interaction name opts
      Just (ComponentData customId _) -> do
        putStrLn $ "Component: " <> T.unpack customId
        void $ Interaction.acknowledge client (interactionId interaction) (interactionToken interaction)
      Nothing -> pure ()
  _ -> pure ()

handleSlashCommand :: Client -> Interaction -> T.Text -> [(T.Text, CommandOptionValue)] -> IO ()
handleSlashCommand client interaction "ping" _ =
  void $
    Interaction.respond client (interactionId interaction) (interactionToken interaction) $
      content "Pong! 🏓"
handleSlashCommand client interaction "player" opts = do
  let playerName = case lookup "name" opts of
        Just (StringValue n) -> n
        _ -> "Unknown"
  void $ Interaction.respond client (interactionId interaction) (interactionToken interaction) $ do
    embed $ do
      embedTitle $ "Player: " <> playerName
      embedDescription "Player lookup coming soon..."
      embedColor 0xF5A623
    actionRow $
      linkButton "View Profile" ("https://krunker.io/social.html?p=profile&q=" <> playerName)
handleSlashCommand client interaction cmd _ =
  void $
    Interaction.respond client (interactionId interaction) (interactionToken interaction) $
      content $
        "Unknown command: " <> cmd
