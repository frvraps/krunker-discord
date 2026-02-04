{-# LANGUAGE OverloadedStrings #-}

module Discord.Message
  ( -- Message builder
    MessageBuilder,
    buildMessage,
    content,
    -- Embeds
    EmbedBuilder,
    embed,
    embedTitle,
    embedDescription,
    embedColor,
    embedField,
    embedFooter,
    embedImage,
    embedThumbnail,
    embedAuthor,
    -- Components
    ActionRowBuilder,
    actionRow,
    -- Buttons
    ButtonStyle (..),
    button,
    linkButton,
    -- Select menus
    SelectOptionBuilder,
    stringSelect,
    selectOption,
    selectOptionDescription,
    selectOptionEmoji,
    selectOptionDefault,
    selectPlaceholder,
    selectMinValues,
    selectMaxValues,
  )
where

import Control.Applicative ((<|>))
import Control.Monad.Writer
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Text (Text)

-- Message

data MessageData = MessageData
  { messageContent :: Maybe Text,
    messageEmbeds :: [EmbedData],
    messageComponents :: [ActionRowData]
  }

instance Semigroup MessageData where
  a <> b =
    MessageData
      { messageContent = messageContent b <|> messageContent a,
        messageEmbeds = messageEmbeds a <> messageEmbeds b,
        messageComponents = messageComponents a <> messageComponents b
      }

instance Monoid MessageData where
  mempty = MessageData Nothing [] []

instance ToJSON MessageData where
  toJSON msg =
    object $
      maybe [] (\c -> ["content" .= c]) (messageContent msg)
        <> (["embeds" .= messageEmbeds msg | not (null (messageEmbeds msg))])
        <> (["components" .= messageComponents msg | not (null (messageComponents msg))])

type MessageBuilder = Writer MessageData ()

buildMessage :: MessageBuilder -> MessageData
buildMessage = execWriter

content :: Text -> MessageBuilder
content t = tell $ mempty {messageContent = Just t}

-- Embeds

data EmbedData = EmbedData
  { embedDataTitle :: Maybe Text,
    embedDataDescription :: Maybe Text,
    embedDataColor :: Maybe Int,
    embedDataFields :: [FieldData],
    embedDataFooter :: Maybe FooterData,
    embedDataImage :: Maybe Text,
    embedDataThumbnail :: Maybe Text,
    embedDataAuthor :: Maybe AuthorData
  }

instance Semigroup EmbedData where
  a <> b =
    EmbedData
      { embedDataTitle = embedDataTitle b <|> embedDataTitle a,
        embedDataDescription = embedDataDescription b <|> embedDataDescription a,
        embedDataColor = embedDataColor b <|> embedDataColor a,
        embedDataFields = embedDataFields a <> embedDataFields b,
        embedDataFooter = embedDataFooter b <|> embedDataFooter a,
        embedDataImage = embedDataImage b <|> embedDataImage a,
        embedDataThumbnail = embedDataThumbnail b <|> embedDataThumbnail a,
        embedDataAuthor = embedDataAuthor b <|> embedDataAuthor a
      }

instance Monoid EmbedData where
  mempty = EmbedData Nothing Nothing Nothing [] Nothing Nothing Nothing Nothing

instance ToJSON EmbedData where
  toJSON e =
    object $
      maybe [] (\t -> ["title" .= t]) (embedDataTitle e)
        <> maybe [] (\d -> ["description" .= d]) (embedDataDescription e)
        <> maybe [] (\c -> ["color" .= c]) (embedDataColor e)
        <> (["fields" .= embedDataFields e | not (null (embedDataFields e))])
        <> maybe [] (\f -> ["footer" .= f]) (embedDataFooter e)
        <> maybe [] (\i -> ["image" .= object ["url" .= i]]) (embedDataImage e)
        <> maybe [] (\t -> ["thumbnail" .= object ["url" .= t]]) (embedDataThumbnail e)
        <> maybe [] (\a -> ["author" .= a]) (embedDataAuthor e)

data FieldData = FieldData
  { fieldName :: Text,
    fieldValue :: Text,
    fieldInline :: Bool
  }

instance ToJSON FieldData where
  toJSON f =
    object
      [ "name" .= fieldName f,
        "value" .= fieldValue f,
        "inline" .= fieldInline f
      ]

data FooterData = FooterData
  { footerText :: Text,
    footerIcon :: Maybe Text
  }

instance ToJSON FooterData where
  toJSON f =
    object $
      ["text" .= footerText f]
        <> maybe [] (\i -> ["icon_url" .= i]) (footerIcon f)

data AuthorData = AuthorData
  { authorName :: Text,
    authorIcon :: Maybe Text,
    authorUrl :: Maybe Text
  }

instance ToJSON AuthorData where
  toJSON a =
    object $
      ["name" .= authorName a]
        <> maybe [] (\i -> ["icon_url" .= i]) (authorIcon a)
        <> maybe [] (\u -> ["url" .= u]) (authorUrl a)

type EmbedBuilder = Writer EmbedData ()

embed :: EmbedBuilder -> MessageBuilder
embed builder = tell $ mempty {messageEmbeds = [execWriter builder]}

embedTitle :: Text -> EmbedBuilder
embedTitle t = tell $ mempty {embedDataTitle = Just t}

embedDescription :: Text -> EmbedBuilder
embedDescription t = tell $ mempty {embedDataDescription = Just t}

embedColor :: Int -> EmbedBuilder
embedColor c = tell $ mempty {embedDataColor = Just c}

embedField :: Text -> Text -> Bool -> EmbedBuilder
embedField name value inline = tell $ mempty {embedDataFields = [FieldData name value inline]}

embedFooter :: Text -> Maybe Text -> EmbedBuilder
embedFooter text icon = tell $ mempty {embedDataFooter = Just $ FooterData text icon}

embedImage :: Text -> EmbedBuilder
embedImage url = tell $ mempty {embedDataImage = Just url}

embedThumbnail :: Text -> EmbedBuilder
embedThumbnail url = tell $ mempty {embedDataThumbnail = Just url}

embedAuthor :: Text -> Maybe Text -> Maybe Text -> EmbedBuilder
embedAuthor name icon url = tell $ mempty {embedDataAuthor = Just $ AuthorData name icon url}

-- Components

data ActionRowData = ActionRowData
  { actionRowComponents :: [ComponentData]
  }

instance ToJSON ActionRowData where
  toJSON r =
    object
      [ "type" .= (1 :: Int),
        "components" .= actionRowComponents r
      ]

type ActionRowBuilder = Writer [ComponentData] ()

actionRow :: ActionRowBuilder -> MessageBuilder
actionRow builder = tell $ mempty {messageComponents = [ActionRowData $ execWriter builder]}

-- Buttons

data ButtonStyle = Primary | Secondary | Success | Danger
  deriving (Show, Eq)

buttonStyleToInt :: ButtonStyle -> Int
buttonStyleToInt Primary = 1
buttonStyleToInt Secondary = 2
buttonStyleToInt Success = 3
buttonStyleToInt Danger = 4

data ComponentData
  = ButtonComponent
      { buttonStyle :: Int,
        buttonLabel :: Maybe Text,
        buttonCustomId :: Maybe Text,
        buttonUrl :: Maybe Text,
        buttonDisabled :: Bool
      }
  | SelectComponent
      { selectCustomId :: Text,
        selectOptions :: [SelectOptionData],
        selectPlaceholderText :: Maybe Text,
        selectMin :: Maybe Int,
        selectMax :: Maybe Int,
        selectDisabled :: Bool
      }

instance ToJSON ComponentData where
  toJSON (ButtonComponent style label customId url disabled) =
    object $
      [ "type" .= (2 :: Int),
        "style" .= style
      ]
        <> maybe [] (\l -> ["label" .= l]) label
        <> maybe [] (\c -> ["custom_id" .= c]) customId
        <> maybe [] (\u -> ["url" .= u]) url
        <> (["disabled" .= True | disabled])
  toJSON (SelectComponent customId options placeholder minV maxV disabled) =
    object $
      [ "type" .= (3 :: Int),
        "custom_id" .= customId,
        "options" .= options
      ]
        <> maybe [] (\p -> ["placeholder" .= p]) placeholder
        <> maybe [] (\m -> ["min_values" .= m]) minV
        <> maybe [] (\m -> ["max_values" .= m]) maxV
        <> (["disabled" .= True | disabled])

button :: ButtonStyle -> Text -> Text -> ActionRowBuilder
button style label customId =
  tell [ButtonComponent (buttonStyleToInt style) (Just label) (Just customId) Nothing False]

linkButton :: Text -> Text -> ActionRowBuilder
linkButton label url =
  tell [ButtonComponent 5 (Just label) Nothing (Just url) False]

-- Select Menus

data SelectOptionData = SelectOptionData
  { optionLabel :: Text,
    optionValue :: Text,
    optionDescription :: Maybe Text,
    optionEmoji :: Maybe Text,
    optionDefault :: Bool
  }

instance ToJSON SelectOptionData where
  toJSON o =
    object $
      [ "label" .= optionLabel o,
        "value" .= optionValue o
      ]
        <> maybe [] (\d -> ["description" .= d]) (optionDescription o)
        <> maybe [] (\e -> ["emoji" .= object ["name" .= e]]) (optionEmoji o)
        <> (["default" .= True | optionDefault o])

data SelectData = SelectData
  { selectDataOptions :: [SelectOptionData],
    selectDataPlaceholder :: Maybe Text,
    selectDataMin :: Maybe Int,
    selectDataMax :: Maybe Int
  }

instance Semigroup SelectData where
  a <> b =
    SelectData
      { selectDataOptions = selectDataOptions a <> selectDataOptions b,
        selectDataPlaceholder = selectDataPlaceholder b <|> selectDataPlaceholder a,
        selectDataMin = selectDataMin b <|> selectDataMin a,
        selectDataMax = selectDataMax b <|> selectDataMax a
      }

instance Monoid SelectData where
  mempty = SelectData [] Nothing Nothing Nothing

type SelectBuilder = Writer SelectData ()

stringSelect :: Text -> SelectBuilder -> ActionRowBuilder
stringSelect customId builder =
  let sd = execWriter builder
   in tell
        [ SelectComponent
            customId
            (selectDataOptions sd)
            (selectDataPlaceholder sd)
            (selectDataMin sd)
            (selectDataMax sd)
            False
        ]

type SelectOptionBuilder = Writer SelectOptionData ()

selectOption :: Text -> Text -> SelectOptionBuilder -> SelectBuilder
selectOption label value builder =
  let base = SelectOptionData label value Nothing Nothing False
      opt = execWriter builder
   in tell $ mempty {selectDataOptions = [base <> opt]}

instance Semigroup SelectOptionData where
  a <> b =
    SelectOptionData
      { optionLabel = optionLabel b,
        optionValue = optionValue b,
        optionDescription = optionDescription b <|> optionDescription a,
        optionEmoji = optionEmoji b <|> optionEmoji a,
        optionDefault = optionDefault b || optionDefault a
      }

instance Monoid SelectOptionData where
  mempty = SelectOptionData "" "" Nothing Nothing False

selectOptionDescription :: Text -> SelectOptionBuilder
selectOptionDescription d = tell $ mempty {optionDescription = Just d}

selectOptionEmoji :: Text -> SelectOptionBuilder
selectOptionEmoji e = tell $ mempty {optionEmoji = Just e}

selectOptionDefault :: SelectOptionBuilder
selectOptionDefault = tell $ mempty {optionDefault = True}

selectPlaceholder :: Text -> SelectBuilder
selectPlaceholder p = tell $ mempty {selectDataPlaceholder = Just p}

selectMinValues :: Int -> SelectBuilder
selectMinValues n = tell $ mempty {selectDataMin = Just n}

selectMaxValues :: Int -> SelectBuilder
selectMaxValues n = tell $ mempty {selectDataMax = Just n}
