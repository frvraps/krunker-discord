# krunker-discord

A Discord bot for Krunker stats, built in Haskell.

## Contributing

See the [Krunker FRVR Code Contribution Document](https://www.notion.so/frvr/Krunker-FRVR-Code-Contribution-Document-aeee93064720475abd4e32cad8a1c5b0) for contribution guidelines.

## Architecture

```
src/
├── Main.hs                      # Entry point and event handling
├── Discord/
│   ├── Client.hs                # HTTP client for REST API
│   ├── Api.hs                   # Base request helpers
│   ├── Gateway.hs               # WebSocket connection to Discord
│   ├── State.hs                 # Bot state (session, heartbeat)
│   ├── Types.hs                 # Gateway types and events
│   ├── Message.hs               # Message builder DSL
│   └── Endpoints/
│       └── Channel.hs           # Channel API endpoints
```

### Gateway

The gateway handles the WebSocket connection to Discord. It manages:

- Connection and reconnection with exponential backoff
- Heartbeating to keep the connection alive
- Session resumption after disconnects
- Dispatching events to your handler

```haskell
main = do
  token <- T.pack <$> getEnv "DISCORD_TOKEN"
  client <- newClient token
  state <- newBotState
  connectAndRun token state (handleEvent client)

handleEvent :: Client -> Event -> IO ()
handleEvent client event = case event of
  ReadyEvent {} -> putStrLn "Ready!"
  MessageCreateEvent {msgContent, msgChannelId} -> do
    Channel.sendMessage client msgChannelId $ content "Hello!"
  UnknownEvent name _ -> putStrLn $ "Unknown: " <> T.unpack name
```

### Events

Events are typed as a sum type:

```haskell
data Event
  = ReadyEvent { readySessionId, readyResumeGatewayUrl }
  | MessageCreateEvent { msgId, msgChannelId, msgGuildId, msgContent, msgAuthor }
  | UnknownEvent Text Value
```

### REST API

The API follows the same pattern as `krunker-hs`:

```haskell
-- Create a client once
client <- newClient token

-- Make requests
result <- Channel.sendMessage client channelId messageBuilder
case result of
  Left err -> print err
  Right () -> pure ()
```

## Message Builder

Messages are constructed using a Writer monad DSL.

### Basic Message

```haskell
Channel.sendMessage client channelId $ do
  content "Hello, world!"
```

### Embeds

```haskell
Channel.sendMessage client channelId $ do
  content "Check this out:"
  embed $ do
    embedTitle "My Embed"
    embedDescription "A description here"
    embedColor 0x5865F2  -- Discord blurple
    embedField "Field 1" "Value 1" True   -- inline
    embedField "Field 2" "Value 2" True   -- inline
    embedField "Field 3" "Value 3" False  -- not inline
    embedFooter "Footer text" (Just "https://example.com/icon.png")
    embedImage "https://example.com/image.png"
    embedThumbnail "https://example.com/thumb.png"
    embedAuthor "Author Name" (Just "https://example.com/avatar.png") (Just "https://example.com")
```

### Buttons

Buttons must be inside an action row. Max 5 buttons per row, max 5 rows per message.

```haskell
Channel.sendMessage client channelId $ do
  content "Click a button:"
  actionRow $ do
    button Primary "Primary" "btn_primary"
    button Secondary "Secondary" "btn_secondary"
    button Success "Success" "btn_success"
    button Danger "Danger" "btn_danger"
  actionRow $ do
    linkButton "Visit Website" "https://example.com"
```

Button styles:
- `Primary` - Blurple
- `Secondary` - Gray
- `Success` - Green
- `Danger` - Red

Link buttons navigate to a URL and don't send an interaction.

### Select Menus

Select menus take up an entire action row.

```haskell
Channel.sendMessage client channelId $ do
  content "Choose an option:"
  actionRow $ do
    stringSelect "my_select" $ do
      selectPlaceholder "Select an option..."
      selectMinValues 1
      selectMaxValues 1
      selectOption "Option A" "a" $ do
        selectOptionDescription "This is option A"
        selectOptionEmoji "🅰️"
      selectOption "Option B" "b" $ do
        selectOptionDescription "This is option B"
        selectOptionEmoji "🅱️"
        selectOptionDefault  -- pre-selected
      selectOption "Option C" "c" $ pure ()
```

### Complete Example

```haskell
Channel.sendMessage client channelId $ do
  content "Welcome to the server!"

  embed $ do
    embedTitle "Getting Started"
    embedDescription "Here's how to use the bot:"
    embedColor 0x00FF00
    embedField "Commands" "`/stats` - View your stats" False
    embedField "Support" "Click the button below" False
    embedFooter "Krunker Discord Bot" Nothing

  actionRow $ do
    button Primary "View Stats" "view_stats"
    button Secondary "Settings" "settings"
    linkButton "Documentation" "https://docs.example.com"

  actionRow $ do
    stringSelect "quick_actions" $ do
      selectPlaceholder "Quick actions..."
      selectOption "Check rank" "rank" $ selectOptionEmoji "🏆"
      selectOption "View matches" "matches" $ selectOptionEmoji "🎮"
      selectOption "Get help" "help" $ selectOptionEmoji "❓"
```

## Running

```bash
export DISCORD_TOKEN="your-bot-token"
cabal run
```

---

*This README was AI-generated from a given spec.*
