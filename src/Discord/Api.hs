{-# LANGUAGE OverloadedStrings #-}

module Discord.Api where

import Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text, unpack)
import Data.Text.Encoding (encodeUtf8)
import Discord.Client
import Network.HTTP.Client hiding (path)
import Network.HTTP.Types.Status

data ApiError = ApiError
  { errorStatus :: Status,
    errorResponse :: LBS.ByteString
  }
  deriving (Show)

makePostRequest :: ToJSON a => Client -> Text -> a -> IO (Either ApiError LBS.ByteString)
makePostRequest client path body = do
  initRequest <- parseRequest $ unpack $ clientBaseUrl client <> path
  let request =
        initRequest
          { method = "POST",
            requestHeaders =
              [ ("Authorization", "Bot " <> encodeUtf8 (clientToken client)),
                ("Content-Type", "application/json")
              ],
            requestBody = RequestBodyLBS $ encode body
          }
  response <- httpLbs request $ clientManager client
  if statusIsSuccessful $ responseStatus response
    then pure $ Right $ responseBody response
    else pure $ Left $ ApiError (responseStatus response) (responseBody response)

postAndDecode :: (ToJSON a, FromJSON b) => Client -> Text -> a -> IO (Either ApiError b)
postAndDecode client path body = do
  res <- makePostRequest client path body
  pure $ res >>= \respBody ->
    maybe (Left $ ApiError status400 "Failed to decode") Right (decode respBody)

makeGetRequest :: Client -> Text -> IO (Either ApiError LBS.ByteString)
makeGetRequest client path = do
  initRequest <- parseRequest $ unpack $ clientBaseUrl client <> path
  let request =
        initRequest
          { requestHeaders =
              [ ("Authorization", "Bot " <> encodeUtf8 (clientToken client))
              ]
          }
  response <- httpLbs request $ clientManager client
  if statusIsSuccessful $ responseStatus response
    then pure $ Right $ responseBody response
    else pure $ Left $ ApiError (responseStatus response) (responseBody response)

getAndDecode :: FromJSON a => Client -> Text -> IO (Either ApiError a)
getAndDecode client path = do
  res <- makeGetRequest client path
  pure $ res >>= \body ->
    maybe (Left $ ApiError status400 "Failed to decode") Right (decode body)
