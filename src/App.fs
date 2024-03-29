module App
open FsharpMyExtension
open FsharpMyExtension.Either
open Microsoft.Extensions.Logging
open System.Threading.Tasks
open DSharpPlus
open DiscordBotExtensions
open DiscordBotExtensions.Types
open DiscordBotExtensions.Extensions

let botEventId = new EventId(42, "Bot-Event")

let initBotModules (db: MongoDB.Driver.IMongoDatabase) (logger: ILogger<_>) =
    [|
        CustomCommand.Main.create db
        Doorkeeper.Main.create db
        Doorkeeper.Invites.create db
        UserRole.Main.create db
        Ranking.Main.create db logger
        VoiceChannelNotification.Main.create db

        MessageManager.create ()
        ReactionEvent.Main.create db
        Events.Main.create db
        ChatVoice.Main.create ()
        Boosters.Main.create db
        UserInfo.Main.create ()
        EggBattle.Main.create db
        Moderation.Main.create ()
        Ship.Main.create ()
        EmojiFont.Main.create ()
        Calc.Main.create ()
        Roll.Main.create ()
        Age.Main.create "age" db
        RpDiscordBot.Core.Characters.Main.create "characters" db
        BallotBox.Main.create (fun setting client e -> AppsHub.start (AppsHub.Hub.InitBallotBox setting) client e)
        NumberToWords.Main.create ()
        EmojiManager.Main.create ()
        ImageChannel.Main.create db
        AppsHub.BotModule.create ()
    |]

open MongoDB.Driver
let initDb () =
    let dbConnection = EnvironmentExt.getEnvironmentVariable "DbConnection"

    let settings =
        MongoClientSettings.FromConnectionString (dbConnection)

    let client = new MongoClient(settings)
    let database =
        let dataBaseName =
            EnvironmentExt.getEnvironmentVariable "DataBaseName"

        client.GetDatabase(dataBaseName)

    database

[<EntryPoint>]
let main argv =
    let tokenEnvVar =
        #if TEST_BOT
        "TestBotToken"
        #else
        "DiscordCommandBotToken"
        #endif

    let ablyToken =
        let serverConnectionVarName =
            #if TEST_BOT
            "AblyTokenTest"
            #else
            "AblyToken"
            #endif
        EnvironmentExt.getEnvironmentVariable serverConnectionVarName

    // Api.start ablyToken

    match EnvironmentExt.tryGetEnvironmentVariable tokenEnvVar with
    | None ->
        printfn "Environment variable `%s` is not set!" tokenEnvVar
        1
    | Some token ->
        let createConfig () =
            let config = DSharpPlus.DiscordConfiguration()

            config.set_Token token
            config.set_TokenType DSharpPlus.TokenType.Bot
            config.set_AutoReconnect true
            config.set_Intents (
                DSharpPlus.DiscordIntents.AllUnprivileged
                ||| DSharpPlus.DiscordIntents.GuildMembers
                ||| DSharpPlus.DiscordIntents.GuildPresences
                ||| DSharpPlus.DiscordIntents.MessageContents
            )
            config

        let client = new DSharpPlus.DiscordClient(createConfig ())
        let rest = new DSharpPlus.DiscordRestClient(createConfig ())

        let botOwner =
            awaiti <| rest.InitializeAsync ()
            rest.CurrentApplication.Owners
            |> Seq.tryHead

        let database = initDb ()
        let botModules = initBotModules database client.Logger

        let prefix = "."

        botModules
        |> BotModule.bindToClientsEvents
            prefix
            (fun client e ->
                let b = DSharpPlus.Entities.DiscordEmbedBuilder()
                b.Description <-
                    [
                        "Доступные команды:"
                        "`.команды` — кастомные команды, доступные на этом сервере"
                        "`.numberToWords <число>` — возвращает число словами, например, `.numberToWords 21435` выдаст:"
                        "```"
                        "двадцать одна тысяча четыреста тридцать пять```"
                        "`.avatar @user` — аватарка указанного пользователя"
                    ] |> String.concat "\n"

                b.Color <- DSharpPlus.Entities.Optional.FromValue(DiscordEmbed.backgroundColorDarkTheme)
                awaiti (client.SendMessageAsync (e.Channel, b.Build()))
            )
            (fun client e ->
                ()
            )
            AppsHub.resp
            client

        client.add_Ready(Emzi0767.Utilities.AsyncEventHandler (fun client readyEventArgs ->
            client.Logger.LogInformation(botEventId, "Client is ready to process events.")

            Task.CompletedTask
        ))

        client.add_ClientErrored(Emzi0767.Utilities.AsyncEventHandler (fun client e ->
            client.Logger.LogError(botEventId, e.Exception, "Exception occured", [||])

            let getBotOwner () =
                match botOwner with
                | Some botOwner ->
                    try
                        await <| rest.CreateDmAsync botOwner.Id
                        |> Ok
                    with e ->
                        Error e
                | None ->
                    Error (new System.Exception("not found bot owner"))

            match getBotOwner () with
            | Ok channel ->
                let embed = Entities.DiscordEmbedBuilder()
                embed.Color <- Entities.Optional.FromValue(DiscordEmbed.backgroundColorDarkTheme)
                embed.Description <-
                    sprintf "```\n%A\n```" e.Exception

                let b = Entities.DiscordMessageBuilder()
                b.Embed <- embed.Build()

                awaiti <| channel.SendMessageAsync b
            | Error x ->
                printfn "get bot owner returned error:\n%A" x

            Task.CompletedTask
        ))

        awaiti <| client.ConnectAsync()

        awaiti <| Task.Delay -1

        0
