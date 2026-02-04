{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Discord.Types
  ( Incoming (..),
    Outgoing (..),
    IdentifyData (..),
    IdentifyProperties (..),
    HelloData (..),
    HeartbeatData (..),
    ResumeData (..),
    GatewayException (..),
    Author (..),
    Event (..),
    Interaction (..),
    InteractionData (..),
    InteractionType (..),
    CommandOptionValue (..),
    parseEvent,
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
import Data.Aeson (FromJSON (parseJSON), Value (..), camelTo2, genericParseJSON, object, (.:), (.:?), (.=))
import Data.Aeson.Types (Options (..), ToJSON (..), defaultOptions, parseMaybe)
import Data.Foldable (toList)
import Data.Maybe (mapMaybe)
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

data Author = Author
  { authorId :: Text,
    authorUsername :: Text,
    authorBot :: Maybe Bool
  }
  deriving (Show, Generic)

instance FromJSON Author where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop 6
        }

data Event
  = ReadyEvent
      { readySessionId :: Text,
        readyResumeGatewayUrl :: Text
      }
  | MessageCreateEvent
      { msgId :: Text,
        msgChannelId :: Text,
        msgGuildId :: Maybe Text,
        msgContent :: Text,
        msgAuthor :: Author
      }
  | InteractionCreateEvent Interaction
  | UnknownEvent Text Value
  deriving (Show)

data InteractionType
  = ApplicationCommand
  | ComponentInteraction
  | OtherInteraction Int
  deriving (Show, Eq)

data CommandOptionValue
  = StringValue Text
  | IntValue Int
  | BoolValue Bool
  | UserValue Text
  | ChannelValue Text
  | RoleValue Text
  deriving (Show)

data InteractionData
  = SlashCommandData
      { commandName :: Text,
        commandOptions :: [(Text, CommandOptionValue)]
      }
  | ComponentData
      { componentCustomId :: Text,
        componentType :: Int
      }
  deriving (Show)

data Interaction = Interaction
  { interactionId :: Text,
    interactionToken :: Text,
    interactionType :: InteractionType,
    interactionChannelId :: Text,
    interactionGuildId :: Maybe Text,
    interactionData :: Maybe InteractionData,
    interactionMessage :: Maybe Value
  }
  deriving (Show)

parseEvent :: Text -> Value -> Event
parseEvent "READY" (Object o) =
  case parseMaybe parser o of
    Just event -> event
    Nothing -> UnknownEvent "READY" (Object o)
  where
    parser obj = ReadyEvent <$> obj .: "session_id" <*> obj .: "resume_gateway_url"
parseEvent "READY" val = UnknownEvent "READY" val
parseEvent "MESSAGE_CREATE" (Object o) =
  case parseMaybe parser o of
    Just event -> event
    Nothing -> UnknownEvent "MESSAGE_CREATE" (Object o)
  where
    parser obj =
      MessageCreateEvent
        <$> obj .: "id"
        <*> obj .: "channel_id"
        <*> obj .:? "guild_id"
        <*> obj .: "content"
        <*> obj .: "author"
parseEvent "MESSAGE_CREATE" val = UnknownEvent "MESSAGE_CREATE" val
parseEvent "INTERACTION_CREATE" (Object o) =
  case parseMaybe parser o of
    Just event -> InteractionCreateEvent event
    Nothing -> UnknownEvent "INTERACTION_CREATE" (Object o)
  where
    parser obj = do
      iId <- obj .: "id"
      iToken <- obj .: "token"
      iType <- obj .: "type"
      iChannelId <- obj .: "channel_id"
      iGuildId <- obj .:? "guild_id"
      iData <- obj .:? "data"
      iMessage <- obj .:? "message"
      let interType = case (iType :: Int) of
            2 -> ApplicationCommand
            3 -> ComponentInteraction
            n -> OtherInteraction n
      parsedData <- case (iType :: Int, iData) of
        (2, Just (Object dataObj)) -> do
          name <- dataObj .: "name"
          opts <- dataObj .:? "options"
          let parsedOpts = case opts of
                Just (Array arr) -> parseOptions (toList arr)
                _ -> []
          pure $ Just $ SlashCommandData name parsedOpts
        (3, Just (Object dataObj)) -> do
          customId <- dataObj .: "custom_id"
          compType <- dataObj .: "component_type"
          pure $ Just $ ComponentData customId compType
        _ -> pure Nothing
      pure $
        Interaction
          iId
          iToken
          interType
          iChannelId
          iGuildId
          parsedData
          iMessage

    parseOptions :: [Value] -> [(Text, CommandOptionValue)]
    parseOptions = mapMaybe parseOpt
      where
        parseOpt (Object optObj) = do
          optName <- parseMaybe (.: "name") optObj
          optType <- parseMaybe (.: "type") optObj
          val <- case (optType :: Int) of
            3 -> StringValue <$> parseMaybe (.: "value") optObj
            4 -> IntValue <$> parseMaybe (.: "value") optObj
            5 -> BoolValue <$> parseMaybe (.: "value") optObj
            6 -> UserValue <$> parseMaybe (.: "value") optObj
            7 -> ChannelValue <$> parseMaybe (.: "value") optObj
            8 -> RoleValue <$> parseMaybe (.: "value") optObj
            _ -> StringValue <$> parseMaybe (.: "value") optObj
          pure (optName, val)
        parseOpt _ = Nothing
parseEvent "INTERACTION_CREATE" val = UnknownEvent "INTERACTION_CREATE" val
parseEvent name val = UnknownEvent name val

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
