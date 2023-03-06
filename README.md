# Agent Discord Bot
[![Discord Chat](https://img.shields.io/discord/878956153537691658?logo=discord&style=social)](https://discord.gg/dCMuaGJktf)

## Install pre-requisites
* The [.NET Core SDK](https://www.microsoft.com/net/download) 3.1 or higher.

## Build
* For Linux: `./build.sh`
* For Windows: `build.cmd`

## Before starting
Create .env and fill next fields:
```
DiscordCommandBotToken=%your token%
DbConnection=%your connection to DB%
DataBaseName=%your database name%
AblyToken=%your AblyToken%
```

## Testing
```bash
dotnet fake build -t RunTest
```

## Heroku
[![Deploy](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy)
