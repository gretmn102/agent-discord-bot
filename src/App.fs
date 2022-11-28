module App
open FsharpMyExtension
open FsharpMyExtension.Either
open Microsoft.Extensions.Logging
open System.Threading.Tasks
open DSharpPlus.VoiceNext

open Types
open Extensions

let botEventId = new EventId(42, "Bot-Event")

let cmd (client:DSharpPlus.DiscordClient) (e:DSharpPlus.EventArgs.MessageCreateEventArgs) =
    let authorId = e.Author.Id
    let botId = client.CurrentUser.Id

    if authorId <> botId then
        match CommandParser.start botId e.Message.Content with
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
                        "`.take @user` — взять кого-то на ручки"
                        "`.fairytail @user` — почитать кому-то сказку"
                        "`.catail @user` — помахать кому-то хвостом"
                        "`.bully @user` — забуллить кого-то <:Demon_Kingsmile:877678191693692969>"
                        "`.admire @user` — любоваться"
                        "`.battery` — спеть \"Батарейку\""
                        "`.catch @user` — поймать кого-то"
                        "`.angry @user` — разозлиться на кого-то"
                        "`.numberToWords <число>` — говорит число словами, например, `.numberToWords 21435` выдаст:"
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

    Api.start ablyToken

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

        let voice = client.UseVoiceNext()

        client.add_Ready(Emzi0767.Utilities.AsyncEventHandler (fun client readyEventArgs ->
            client.Logger.LogInformation(botEventId, "Client is ready to process events.")

            Task.CompletedTask
        ))

        client.add_GuildAvailable(Emzi0767.Utilities.AsyncEventHandler (fun client e ->
            Doorkeeper.Invites.guildAvailableHandle e.Guild

            Task.CompletedTask
        ))

        client.add_MessageCreated (Emzi0767.Utilities.AsyncEventHandler (fun client e ->
            Ranking.Main.handle e
            Events.Main.handle e
            DiscordWebhook.Main.handle e
            CustomCommand.Main.handle client e

            cmd client e

            Task.CompletedTask
        ))

        client.add_MessageDeleted (Emzi0767.Utilities.AsyncEventHandler (fun client e ->
            ReactionEvent.Main.messageDeletedHandle e

            Task.CompletedTask
        ))

        client.add_GuildRoleDeleted (Emzi0767.Utilities.AsyncEventHandler (fun client e ->
            UserRole.Main.guildRoleDeletedHandler e

            Task.CompletedTask
        ))

        client.add_ClientErrored(Emzi0767.Utilities.AsyncEventHandler (fun client e ->
            client.Logger.LogError(botEventId, e.Exception, "Exception occured", [||])

            Task.CompletedTask
        ))
        client.add_ComponentInteractionCreated (Emzi0767.Utilities.AsyncEventHandler (fun client e ->
            client.Logger.LogInformation(botEventId, "Component created", [||])
            let isHandled =
                Age.Main.componentInteractionCreateHandle client e
                || UserRole.Main.componentInteractionCreateHandle client e
                || Ranking.Main.componentInteractionCreateHandle client e
                || Doorkeeper.Invites.componentInteractionCreateHandle client e
                || EggBattle.Main.componentInteractionCreateHandle client e
                // || Fishing.Main.componentInteractionCreateHandle client e
                // || SimpleQuiz.Main.componentInteractionCreateHandle client e

            if not isHandled then
                AppsHub.resp client e

            Task.CompletedTask
        ))

        client.add_VoiceStateUpdated (Emzi0767.Utilities.AsyncEventHandler (fun client e ->
            ChatVoice.Main.voiceHandle e
            VoiceChannelNotification.Main.voiceHandle e

            Task.CompletedTask
        ))

        client.add_GuildMemberAdded (Emzi0767.Utilities.AsyncEventHandler (fun client e ->
            Doorkeeper.Main.guildMemberAddHandle e
            Doorkeeper.Invites.guildMemberAddedHandle e
            Age.Main.guildMemberAddedHandle e

            Task.CompletedTask
        ))

        client.add_GuildMemberRemoved (Emzi0767.Utilities.AsyncEventHandler (fun client e ->
            Doorkeeper.Main.guildMemberRemoveHandle e
            Age.Main.guildMemberRemoveHandle e

            Task.CompletedTask
        ))

        client.add_GuildMemberUpdated (Emzi0767.Utilities.AsyncEventHandler (fun client e ->
            Boosters.Main.handle e

            Task.CompletedTask
        ))

        client.add_MessageReactionAdded (Emzi0767.Utilities.AsyncEventHandler (fun client e ->
            if client.CurrentUser.Id <> e.User.Id then
                ReactionEvent.Main.handle (ReactionEvent.Main.AddedEvent e)

            Task.CompletedTask
        ))

        client.add_MessageReactionRemoved (Emzi0767.Utilities.AsyncEventHandler (fun client e ->
            if client.CurrentUser.Id <> e.User.Id then
                ReactionEvent.Main.handle (ReactionEvent.Main.RemovedEvent e)

            Task.CompletedTask
        ))

        client.add_InviteCreated (Emzi0767.Utilities.AsyncEventHandler (fun client e ->
            Doorkeeper.Invites.inviteCreatedHandle e

            Task.CompletedTask
        ))

        client.add_InviteDeleted (Emzi0767.Utilities.AsyncEventHandler (fun client e ->
            Doorkeeper.Invites.inviteDeletedHandle e

            Task.CompletedTask
        ))

        client.add_ModalSubmitted (Emzi0767.Utilities.AsyncEventHandler (fun client e ->
            Age.Main.modalHandle e
            UserRole.Main.modalHandle e

            Task.CompletedTask
        ))

        Ranking.Main.mostActiveTimerStart client

        awaiti <| client.ConnectAsync()

        awaiti <| Task.Delay -1

        0