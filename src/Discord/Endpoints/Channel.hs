{-# LANGUAGE OverloadedStrings #-}

module Discord.Endpoints.Channel where

import Data.Text (Text)
import Discord.Api
import Discord.Client
import Discord.Message

sendMessage :: Client -> Text -> MessageBuilder -> IO (Either ApiError ())
sendMessage client channelId builder = do
  res <-
    makePostRequest client ("/channels/" <> channelId <> "/messages") $
      buildMessage builder
  pure $ res >> Right ()
