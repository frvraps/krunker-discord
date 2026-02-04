# krunker-discord

A Discord bot for Krunker stats, built in Haskell.

## Contributing

See the [FRVR Code Contribution Document](https://www.notion.so/frvr/Krunker-FRVR-Code-Contribution-Document-aeee93064720475abd4e32cad8a1c5b0) for contribution guidelines.

## Architecture

The bot uses two separate connections to Discord:

1. **Gateway (WebSocket)** - Receives real-time events (interactions, messages, etc.)
2. **REST API (HTTP)** - Registers commands, sends responses

```
┌─────────────────────────────────────────────────────────────┐
│                         Main.hs                             │
│                    (Event Handler)                          │
└─────────────────┬───────────────────────────┬───────────────┘
                  │                           │
                  ▼                           ▼
┌─────────────────────────────┐   ┌───────────────────────────┐
│     Gateway (WebSocket)     │   │      REST API (HTTP)      │
│                             │   │                           │
│  • Connects to Discord      │   │  • Registers slash cmds   │
│  • Receives HELLO           │   │  • Responds to interactions│
│  • Sends IDENTIFY           │   │  • Message builder creates │
│  • Heartbeats every ~41s    │   │    JSON payloads          │
│  • Receives interactions    │   │                           │
│  • Handles reconnection     │   │                           │
└─────────────────────────────┘   └───────────────────────────┘
```

### Gateway Connection Flow

1. Connect to `wss://gateway.discord.gg`
2. Receive `HELLO` with heartbeat interval
3. Send `IDENTIFY` with bot token
4. Receive `READY` with session info
5. Start heartbeat loop (runs in separate thread via `withAsync`)
6. Enter event loop, dispatch events to handler

On disconnect, the gateway attempts reconnection with exponential backoff. If a session ID exists, it sends `RESUME` instead of `IDENTIFY` to replay missed events.

### Events

Events are parsed from raw JSON into a typed sum type:

```haskell
data Event
  = ReadyEvent { readySessionId, readyResumeGatewayUrl }
  | MessageCreateEvent { msgId, msgChannelId, msgGuildId, msgContent, msgAuthor }
  | InteractionCreateEvent Interaction
  | UnknownEvent Text Value

data InteractionData
  = SlashCommandData { commandName, commandOptions }
  | ComponentData { componentCustomId, componentType }
```

## Slash Commands

### Registering Commands

Commands are registered via the REST API on startup. Use guild commands for development (instant updates) and global commands for production (up to 1 hour to propagate).

```haskell
-- Register guild commands (instant)
Command.registerGuildCommands client appId guildId
  [ Command.SlashCommand "ping" "Check if the bot is alive" [],
    Command.SlashCommand "player" "Look up a Krunker player"
      [ Command.CommandOption "name" "Player name" Command.StringOption True
      ]
  ]

-- Register global commands (up to 1 hour delay)
Command.registerGlobalCommands client appId [...]
```

### Command Options

```haskell
data OptionType
  = StringOption   -- Text input
  | IntegerOption  -- Whole numbers
  | BooleanOption  -- True/False
  | UserOption     -- User mention
  | ChannelOption  -- Channel mention
  | RoleOption     -- Role mention
```

### Handling Commands

Interactions arrive via the gateway as `InteractionCreateEvent`:

```haskell
handleEvent client event = void $ forkIO $ case event of
  InteractionCreateEvent interaction ->
    case interactionData interaction of
      Just (SlashCommandData name opts) ->
        handleSlashCommand client interaction name opts
      Just (ComponentData customId _) ->
        void $ Interaction.acknowledge client (interactionId interaction) (interactionToken interaction)
      Nothing -> pure ()
  _ -> pure ()

handleSlashCommand client interaction "ping" _ =
  void $ Interaction.respond client (interactionId interaction) (interactionToken interaction) $
    content "Pong! 🏓"

handleSlashCommand client interaction "player" opts = do
  let playerName = case lookup "name" opts of
        Just (StringValue n) -> n
        _ -> "Unknown"
  void $ Interaction.respond client (interactionId interaction) (interactionToken interaction) $ do
    embed $ do
      embedTitle $ "Player: " <> playerName
      embedColor 0xF5A623
```

## Message Builder

Messages are constructed using a Writer monad DSL.

### Embeds

```haskell
embed $ do
  embedTitle "My Embed"
  embedDescription "A description here"
  embedColor 0x5865F2
  embedField "Field 1" "Value 1" True
  embedField "Field 2" "Value 2" True
  embedFooter "Footer text" Nothing
  embedImage "https://example.com/image.png"
  embedThumbnail "https://example.com/thumb.png"
```

### Buttons

```haskell
actionRow $ do
  button Primary "Click Me" "btn_click"
  button Secondary "Cancel" "btn_cancel"
  button Success "Confirm" "btn_confirm"
  button Danger "Delete" "btn_delete"
  linkButton "Website" "https://example.com"
```

### Select Menus

```haskell
actionRow $ do
  stringSelect "my_select" $ do
    selectPlaceholder "Choose..."
    selectOption "Option A" "a" $ selectOptionDescription "First option"
    selectOption "Option B" "b" $ selectOptionDefault
```

## Running

```bash
export DISCORD_TOKEN="your-bot-token"
export DISCORD_APP_ID="your-application-id"
export DISCORD_GUILD_ID="your-test-guild-id"
cabal run
```

---

*This README was AI-generated from a given spec.*
