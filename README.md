# Agent Discord Bot
[![Discord Chat](https://img.shields.io/discord/878956153537691658?logo=discord&style=social)](https://discord.gg/dCMuaGJktf)

## Install pre-requisites
* The [.NET Core SDK](https://www.microsoft.com/net/download) 3.1 or higher.

## Starting the application
Before you run the project **for the first time only** you must install dotnet "local tools" with this command:

```bash
dotnet tool restore
dotnet paket install
```

```bash
dotnet fake build -t RunTest
```

## Heroku
[![Deploy](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy)
