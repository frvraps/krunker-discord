{-# LANGUAGE OverloadedStrings #-}

module Discord.Endpoints.Command
  ( SlashCommand (..),
    CommandOption (..),
    OptionType (..),
    registerGlobalCommands,
    registerGuildCommands,
  )
where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Text (Text)
import Discord.Api
import Discord.Client

data OptionType
  = StringOption
  | IntegerOption
  | BooleanOption
  | UserOption
  | ChannelOption
  | RoleOption
  deriving (Show)

optionTypeToInt :: OptionType -> Int
optionTypeToInt StringOption = 3
optionTypeToInt IntegerOption = 4
optionTypeToInt BooleanOption = 5
optionTypeToInt UserOption = 6
optionTypeToInt ChannelOption = 7
optionTypeToInt RoleOption = 8

data CommandOption = CommandOption
  { optionName :: Text,
    optionDescription :: Text,
    optionType :: OptionType,
    optionRequired :: Bool
  }
  deriving (Show)

instance ToJSON CommandOption where
  toJSON opt =
    object
      [ "name" .= optionName opt,
        "description" .= optionDescription opt,
        "type" .= optionTypeToInt (optionType opt),
        "required" .= optionRequired opt
      ]

data SlashCommand = SlashCommand
  { commandName :: Text,
    commandDescription :: Text,
    commandOptions :: [CommandOption]
  }
  deriving (Show)

instance ToJSON SlashCommand where
  toJSON cmd =
    object $
      [ "name" .= commandName cmd,
        "description" .= commandDescription cmd
      ]
        <> (["options" .= commandOptions cmd | not (null (commandOptions cmd))])

-- Register commands globally (takes up to 1 hour to propagate)
registerGlobalCommands :: Client -> Text -> [SlashCommand] -> IO (Either ApiError ())
registerGlobalCommands client appId commands = do
  res <- makePutRequest client ("/applications/" <> appId <> "/commands") commands
  pure $ res >> Right ()

-- Register commands for a specific guild (instant)
registerGuildCommands :: Client -> Text -> Text -> [SlashCommand] -> IO (Either ApiError ())
registerGuildCommands client appId guildId commands = do
  res <- makePutRequest client ("/applications/" <> appId <> "/guilds/" <> guildId <> "/commands") commands
  pure $ res >> Right ()
