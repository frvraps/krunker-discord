{-# LANGUAGE OverloadedStrings #-}

module Discord.Client where

import Data.Text (Text)
import Network.HTTP.Client
import Network.HTTP.Client.TLS

data Client = Client
  { clientBaseUrl :: Text,
    clientManager :: Manager,
    clientToken :: Text
  }

defaultBaseUrl :: Text
defaultBaseUrl = "https://discord.com/api/v10"

newClient :: Text -> IO Client
newClient token = do
  manager <- newManager tlsManagerSettings
  pure $ Client defaultBaseUrl manager token
