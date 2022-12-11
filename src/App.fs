module App
open FsharpMyExtension
open FsharpMyExtension.Either
open Microsoft.Extensions.Logging
open System.Threading.Tasks

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

        // SimpleQuiz.Main.create db
        // Fishing.Main.create db
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
        let config = DSharpPlus.DiscordConfiguration()

        config.set_Token token
        config.set_TokenType DSharpPlus.TokenType.Bot
        config.set_AutoReconnect true
        config.set_Intents (
            DSharpPlus.DiscordIntents.AllUnprivileged
            ||| DSharpPlus.DiscordIntents.GuildMembers
            ||| DSharpPlus.DiscordIntents.GuildPresences
        )

        let client = new DSharpPlus.DiscordClient(config)

        let database = initDb ()
        let botModules = initBotModules database

        let schedulers =
            botModules
            |> Array.choose (fun x ->
                x.Scheduler
            )
        client.add_Ready(Emzi0767.Utilities.AsyncEventHandler (fun client readyEventArgs ->
            client.Logger.LogInformation(botEventId, "Client is ready to process events.")

            schedulers
            |> Array.iter (fun f ->
                let isContinued = f client
                ()
            )

            Task.CompletedTask
        ))

        let guildAvailableHandlers =
            botModules
            |> Array.choose (fun x ->
                x.GuildAvailableHandler
            )
        client.add_GuildAvailable(Emzi0767.Utilities.AsyncEventHandler (fun client e ->
            guildAvailableHandlers
            |> Array.iter (fun f -> f e)

            Task.CompletedTask
        ))

        let parseExcludeCommands =
            let pcommands =
                botModules
                |> Array.choose (fun x ->
                    x.MessageCreateEventHandleExclude
                )
                |> CommandParser.initCommandParser
            CommandParser.start pcommands

        let messageCreateHandlers =
            botModules
            |> Array.choose (fun x ->
                x.MessageCreateEventHandle
            )
        client.add_MessageCreated (Emzi0767.Utilities.AsyncEventHandler (fun client e ->
            cmd parseExcludeCommands client e

            messageCreateHandlers
            |> Array.iter (fun f -> f (client, e))

            Task.CompletedTask
        ))

        let messageDeletedHandlers =
            botModules
            |> Array.choose (fun x ->
                x.MessageDeletedHandler
            )
        client.add_MessageDeleted (Emzi0767.Utilities.AsyncEventHandler (fun client e ->
            messageDeletedHandlers
            |> Array.iter (fun f -> f e)

            Task.CompletedTask
        ))

        let guildRoleDeletedHandlers =
            botModules
            |> Array.choose (fun x ->
                x.GuildRoleDeletedHandler
            )
        client.add_GuildRoleDeleted (Emzi0767.Utilities.AsyncEventHandler (fun client e ->
            guildRoleDeletedHandlers
            |> Array.iter (fun f -> f e)

            Task.CompletedTask
        ))

        client.add_ClientErrored(Emzi0767.Utilities.AsyncEventHandler (fun client e ->
            client.Logger.LogError(botEventId, e.Exception, "Exception occured", [||])

            Task.CompletedTask
        ))

        let componentInteractionCreatedHandlers =
            botModules
            |> Array.choose (fun x ->
                x.ComponentInteractionCreateHandle
            )
            |> List.ofArray
        client.add_ComponentInteractionCreated (Emzi0767.Utilities.AsyncEventHandler (fun client e ->
            client.Logger.LogInformation(botEventId, "Component created", [||])

            let isHandled =
                componentInteractionCreatedHandlers
                |> List.exactlyFold
                    (fun st f ->
                        let st = f (client, e)
                        st, st
                    )
                    false

            if not isHandled then
                AppsHub.resp client e

            Task.CompletedTask
        ))

        let voiceStateUpdatedHandlers =
            botModules
            |> Array.choose (fun x ->
                x.VoiceStateUpdatedHandler
            )
        client.add_VoiceStateUpdated (Emzi0767.Utilities.AsyncEventHandler (fun client e ->
            voiceStateUpdatedHandlers
            |> Array.iter (fun f -> f e)

            Task.CompletedTask
        ))

        let guildMemberAddedHandlers =
            botModules
            |> Array.choose (fun x ->
                x.GuildMemberAddedHandler
            )
        client.add_GuildMemberAdded (Emzi0767.Utilities.AsyncEventHandler (fun client e ->
            guildMemberAddedHandlers
            |> Array.iter (fun f -> f e)

            Task.CompletedTask
        ))

        let guildMemberRemovedHandlers =
            botModules
            |> Array.choose (fun x ->
                x.GuildMemberRemovedHandler
            )
        client.add_GuildMemberRemoved (Emzi0767.Utilities.AsyncEventHandler (fun client e ->
            guildMemberRemovedHandlers
            |> Array.iter (fun f -> f e)

            Task.CompletedTask
        ))

        let guildMemberUpdatedHandlers =
            botModules
            |> Array.choose (fun x ->
                x.GuildMemberUpdatedHandler
            )
        client.add_GuildMemberUpdated (Emzi0767.Utilities.AsyncEventHandler (fun client e ->
            guildMemberUpdatedHandlers
            |> Array.iter (fun f -> f e)

            Task.CompletedTask
        ))

        let messageReactionAddedHandlers =
            botModules
            |> Array.choose (fun x ->
                x.MessageReactionAddedHandler
            )
        client.add_MessageReactionAdded (Emzi0767.Utilities.AsyncEventHandler (fun client e ->
            messageReactionAddedHandlers
            |> Array.iter (fun f -> f (client, e))

            Task.CompletedTask
        ))

        let messageReactionAddedHandlers =
            botModules
            |> Array.choose (fun x ->
                x.MessageReactionRemoved
            )
        client.add_MessageReactionRemoved (Emzi0767.Utilities.AsyncEventHandler (fun client e ->
            messageReactionAddedHandlers
            |> Array.iter (fun f -> f (client, e))

            Task.CompletedTask
        ))

        let inviteCreatedHandlers =
            botModules
            |> Array.choose (fun x ->
                x.InviteCreatedHandler
            )
        client.add_InviteCreated (Emzi0767.Utilities.AsyncEventHandler (fun client e ->
            inviteCreatedHandlers
            |> Array.iter (fun f -> f e)

            Task.CompletedTask
        ))

        let inviteDeletedHandlers =
            botModules
            |> Array.choose (fun x ->
                x.InviteDeletedHandler
            )
        client.add_InviteDeleted (Emzi0767.Utilities.AsyncEventHandler (fun client e ->
            inviteDeletedHandlers
            |> Array.iter (fun f -> f e)

            Task.CompletedTask
        ))

        let componentInteractionCreatedHandlers =
            botModules
            |> Array.choose (fun x ->
                x.ModalSubmit
            )
            |> List.ofArray
        client.add_ModalSubmitted (Emzi0767.Utilities.AsyncEventHandler (fun client e ->
            let isHandled =
                componentInteractionCreatedHandlers
                |> List.exactlyFold
                    (fun st f ->
                        let st = f e
                        st, st
                    )
                    false

            Task.CompletedTask
        ))

        awaiti <| client.ConnectAsync()

        awaiti <| Task.Delay -1

        0
