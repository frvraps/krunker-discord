{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Monad (void, when)
import qualified Data.Text as T
import Discord.Client (Client, newClient)
import qualified Discord.Endpoints.Channel as Channel
import Discord.Gateway (EventHandler, connectAndRun)
import Discord.Message
import Discord.State (newBotState)
import Discord.Types
import qualified Krunker
import System.Environment (getEnv)

main :: IO ()
main = do
  discordToken <- T.pack <$> getEnv "DISCORD_TOKEN"
  krunkerApiKey <- T.pack <$> getEnv "KRUNKER_API_KEY"

  client <- newClient discordToken
  krunkerClient <- Krunker.newClient krunkerApiKey
  state <- newBotState

  connectAndRun discordToken state (handleEvent client krunkerClient)

handleEvent :: Client -> Krunker.Client -> EventHandler
handleEvent client krunkerClient event = void $ forkIO $ case event of
  ReadyEvent {} -> putStrLn "Ready!"
  MessageCreateEvent {msgContent = msg, msgChannelId = chanId, msgAuthor = author} ->
    when (authorBot author /= Just True && ".kb " `T.isPrefixOf` msg) $ do
      let args = T.strip $ T.drop 4 msg
      handleTextCommand client krunkerClient chanId args
  _ -> pure ()

handleTextCommand :: Client -> Krunker.Client -> T.Text -> T.Text -> IO ()
handleTextCommand client krunkerClient chanId args =
  case T.words args of
    ("player" : rest) -> do
      let name = T.unwords rest
      if T.null name
        then void $ Channel.sendMessage client chanId $ content "Usage: `.kb player <name>`"
        else do
          result <- Krunker.getPlayer krunkerClient name
          case result of
            Left _ ->
              void $ Channel.sendMessage client chanId $
                content $ "Could not find player **" <> name <> "**."
            Right player ->
              void $ Channel.sendMessage client chanId $ do
                embed $ do
                  embedTitle $ Krunker.playerName player
                  embedColor 0xF5A623
                  embedThumbnail $ Krunker.playerProfilePicture player
                  let clan = Krunker.playerClan player
                  embedDescription $
                    (if T.null clan then "" else "Clan: **" <> clan <> "**\n")
                      <> "Level **"
                      <> showT (Krunker.playerLevel player)
                      <> "** | "
                      <> formatTimePlayed (Krunker.playerTimePlayed player)
                      <> " played"
                  embedField "Kills" (showT $ Krunker.playerKills player) True
                  embedField "Deaths" (showT $ Krunker.playerDeaths player) True
                  embedField "KDR" (showT $ Krunker.playerKdr player) True
                  embedField "Games" (showT $ Krunker.playerGames player) True
                  embedField "Wins" (showT $ Krunker.playerWins player) True
                  embedField "Losses" (showT $ Krunker.playerLosses player) True
                  embedField "KPG" (showT $ Krunker.playerKpg player) True
                  embedField "Score" (showT $ Krunker.playerScore player) True
                  embedField "SPK" (showT $ Krunker.playerSpk player) True
                  embedField "Nukes" (showT $ Krunker.playerNukes player) True
                  embedField "Assists" (showT $ Krunker.playerAssists player) True
                  embedField "Headshots" (showT $ Krunker.playerHeadshots player) True
                  embedField "Wallbangs" (showT $ Krunker.playerWallbangs player) True
                  embedField "Melees" (showT $ Krunker.playerMelees player) True
                  embedField "Beatdowns" (showT $ Krunker.playerBeatdowns player) True
                  let hitRate =
                        if Krunker.playerShots player == 0
                          then "0%"
                          else showT (round (fromIntegral (Krunker.playerHits player) / fromIntegral (Krunker.playerShots player) * 100 :: Double) :: Int) <> "%"
                  embedField "Hit Rate" hitRate True
                  embedField "Bullseyes" (showT $ Krunker.playerBullseyes player) True
                  embedField "Airdrops" (showT $ Krunker.playerAirdrops player) True
                  embedField "Stolen" (showT $ Krunker.playerAirdropsStolen player) True
                  embedField "Juggernauts" (showT $ Krunker.playerJuggernauts player) True
                  embedField "Jugg. Killed" (showT $ Krunker.playerJuggernautsKilled player) True
                  embedField "Warmachines" (showT $ Krunker.playerWarmachines player) True
                  embedField "Slimes" (showT $ Krunker.playerSlimes player) True
                  embedField "KR" (showT $ Krunker.playerKr player) True
                  embedField "Followers" (showT $ Krunker.playerFollowers player) True
                  embedFooter ("Joined " <> T.takeWhile (/= 'T') (Krunker.playerCreatedAt player)) Nothing
                actionRow $
                  linkButton "View Profile" ("https://krunker.io/social.html?p=profile&q=" <> Krunker.playerName player)
    _ ->
      void $ Channel.sendMessage client chanId $
        content "**Commands:** `.kb player <name>`"

showT :: (Show a) => a -> T.Text
showT = T.pack . show

formatTimePlayed :: Int -> T.Text
formatTimePlayed ms =
  let totalSeconds = ms `div` 1000
      hours = totalSeconds `div` 3600
      mins = (totalSeconds `mod` 3600) `div` 60
   in showT hours <> "h " <> showT mins <> "m"

