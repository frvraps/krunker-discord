{-# LANGUAGE OverloadedStrings #-}

module Discord.Endpoints.Interaction where

import Data.Aeson (object, (.=))
import Data.Text (Text)
import Discord.Api
import Discord.Client
import Discord.Message

-- Response type 4 = Channel message with source
-- Response type 6 = Deferred update (acknowledge without sending new message)
-- Response type 7 = Update message

respond :: Client -> Text -> Text -> MessageBuilder -> IO (Either ApiError ())
respond client interactionId interactionToken builder = do
  res <-
    makePostRequest
      client
      ("/interactions/" <> interactionId <> "/" <> interactionToken <> "/callback")
      $ object
        [ "type" .= (4 :: Int),
          "data" .= buildMessage builder
        ]
  pure $ res >> Right ()

updateMessage :: Client -> Text -> Text -> MessageBuilder -> IO (Either ApiError ())
updateMessage client interactionId interactionToken builder = do
  res <-
    makePostRequest
      client
      ("/interactions/" <> interactionId <> "/" <> interactionToken <> "/callback")
      $ object
        [ "type" .= (7 :: Int),
          "data" .= buildMessage builder
        ]
  pure $ res >> Right ()

acknowledge :: Client -> Text -> Text -> IO (Either ApiError ())
acknowledge client interactionId interactionToken = do
  res <-
    makePostRequest
      client
      ("/interactions/" <> interactionId <> "/" <> interactionToken <> "/callback")
      $ object ["type" .= (6 :: Int)]
  pure $ res >> Right ()
