module App
open FsharpMyExtension
open FsharpMyExtension.Either
open Microsoft.Extensions.Logging
open System.Threading.Tasks
open DSharpPlus

open Types
open Extensions

let botEventId = new EventId(42, "Bot-Event")

let cmd pstart (client: DSharpPlus.DiscordClient) (e: DSharpPlus.EventArgs.MessageCreateEventArgs) =
    let authorId = e.Author.Id
    let botId = client.CurrentUser.Id

    if authorId <> botId then
        match pstart botId e.Message.Content with
        | Right res ->
            match res with
            | CommandParser.Pass -> ()

            | CommandParser.Cyoa x ->
                AppsHub.start (AppsHub.Hub.InitCyoa x) client e
            | CommandParser.SomeQuiz ->
                AppsHub.start AppsHub.Hub.InitQuiz client e

            | CommandParser.Unknown ->
                let b = DSharpPlus.Entities.DiscordEmbedBuilder()
                b.Description <-
                    [
                        "Неизвестная команда. Доступные команды:"
                        "`.команды` — кастомные команды, доступные на этом сервере"
                        "`.numberToWords <число>` — возвращает число словами, например, `.numberToWords 21435` выдаст:"
                        "```"
                        "двадцать одна тысяча четыреста тридцать пять```"
                        "`.avatar @user` — аватарка указанного пользователя"
                    ] |> String.concat "\n"

                b.Color <- DSharpPlus.Entities.Optional.FromValue(DiscordEmbed.backgroundColorDarkTheme)
                awaiti (client.SendMessageAsync (e.Channel, b.Build()))

            | CommandParser.MessageCreateEventHandler exec ->
                exec (client, e)

        | Left x ->
            awaiti (client.SendMessageAsync (e.Channel, (sprintf "Ошибка:\n```\n%s\n```" x)))

let initBotModules (db: MongoDB.Driver.IMongoDatabase) =
    [|
        CustomCommand.Main.create db
        Doorkeeper.Main.create db
        Doorkeeper.Invites.create db
        UserRole.Main.create db
        Ranking.Main.create db
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
        DiscordWebhook.Main.create "characters" db
        BallotBox.Main.create (fun setting client e -> AppsHub.start (AppsHub.Hub.InitBallotBox setting) client e)
        NumberToWords.Main.create ()
        EmojiManager.Main.create ()

        // SimpleQuiz.Main.create db
    |]

open MongoDB.Driver
let initDb () =
    let login = getEnvironmentVariable "BotDbL"
    let password = getEnvironmentVariable "BotDbP"

    let settings =
        MongoClientSettings.FromConnectionString (
            sprintf
                "mongodb+srv://%s:%s@cluster0.jkwib.mongodb.net/myFirstDatabase?retryWrites=true&w=majority"
                login
                password
        )

    let client = new MongoClient(settings)
    let database =
        let dataBaseName =
            getEnvironmentVariable "DataBaseName"

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
        getEnvironmentVariable serverConnectionVarName

    // Api.start ablyToken

    match tryGetEnvironmentVariable tokenEnvVar with
    | None ->
        printfn "Environment variable `%s` is not set!" tokenEnvVar
        1
    | Some token ->
        let createConfig () =
            let config = DiscordConfiguration()
            config.set_Token token
            config

        let rest = new DiscordRestClient(createConfig ())

        let botEventId = EventId(1, "botEventId")
        let botOwnerEventId = EventId(2, "botOwner")

        let startClient botOwner =
            let client = new DiscordClient(createConfig ())
            client.add_Ready(Emzi0767.Utilities.AsyncEventHandler (fun client readyEventArgs ->
                client.Logger.LogInformation(botEventId, "Client is ready to process events.")
                client.Logger.LogInformation(botOwnerEventId, sprintf "%A" botOwner)
                Task.CompletedTask
            ))

            client.ConnectAsync()

        let main =
            async {
                do! rest.InitializeAsync () |> Async.AwaitTask
                let botOwner = rest.CurrentApplication.Owners |> Seq.tryHead
                do! startClient botOwner |> Async.AwaitTask
                do! Task.Delay -1 |> Async.AwaitTask
            }

        main |> Async.RunSynchronously

        0
