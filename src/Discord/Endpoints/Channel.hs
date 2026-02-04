{-# LANGUAGE OverloadedStrings #-}

module Discord.Endpoints.Channel where

import Data.Aeson (object, (.=))
import Data.Text (Text)
import Discord.Api
import Discord.Client

sendMessage :: Client -> Text -> Text -> IO (Either ApiError ())
sendMessage client channelId content = do
  res <- makePostRequest client ("/channels/" <> channelId <> "/messages") $
    object ["content" .= content]
  pure $ res >> Right ()
