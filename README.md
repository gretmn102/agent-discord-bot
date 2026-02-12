# Agent Discord Bot
[![Discord Chat](https://img.shields.io/discord/878956153537691658?logo=discord&style=social)](https://discord.gg/dCMuaGJktf)

## Install pre-requisites
* The [.NET 8 SDK](https://dotnet.microsoft.com/download/dotnet/8.0) or higher.

## Build
* For Linux: `./build.sh`
* For Windows: `build.cmd`

Alternatively, without FAKE:
```bash
dotnet paket install
dotnet build src/MainProj.fsproj -c Release
```
(The FSharpMyExt package is in `nuget-local/` and committed to the repo.)

## Before starting
Create .env and fill next fields:
```
DiscordCommandBotToken=%your token%
DbConnection=%your connection to DB%
DataBaseName=%your database name%
```

## Testing
```bash
dotnet run --project tests/Tests.fsproj -c Release
```

## Run bot
```bash
dotnet run --project src/MainProj.fsproj -c Release
```

## Heroku
[![Deploy](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy)
