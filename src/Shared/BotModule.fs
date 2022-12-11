module Shared.BotModule
open DSharpPlus
open FParsec
open System.Threading.Tasks
open FsharpMyExtension
open Microsoft.Extensions.Logging

open Types

let botEventId = new EventId(1, "Bot-Module")

type 'a Parser = Parser<'a, unit>

type MessageCreateEventHandler = ((DiscordClient * EventArgs.MessageCreateEventArgs) -> unit)

type BotModule =
    {
        MessageCreateEventHandleExclude: Option<MessageCreateEventHandler Parser>
        MessageCreateEventHandle: Option<DiscordClient * EventArgs.MessageCreateEventArgs -> unit>
        ComponentInteractionCreateHandle: Option<DiscordClient * EventArgs.ComponentInteractionCreateEventArgs -> bool>
        ModalSubmit: Option<EventArgs.ModalSubmitEventArgs -> bool>
        GuildRoleDeletedHandler: Option<EventArgs.GuildRoleDeleteEventArgs -> unit>
        GuildMemberAddedHandler: Option<EventArgs.GuildMemberAddEventArgs -> unit>
        GuildMemberRemovedHandler: Option<EventArgs.GuildMemberRemoveEventArgs -> unit>
        GuildMemberUpdatedHandler: Option<EventArgs.GuildMemberUpdateEventArgs -> unit>
        MessageReactionAddedHandler: Option<DiscordClient * EventArgs.MessageReactionAddEventArgs -> unit>
        MessageReactionRemoved: Option<DiscordClient * EventArgs.MessageReactionRemoveEventArgs -> unit>
        MessageDeletedHandler: Option<EventArgs.MessageDeleteEventArgs -> unit>
        VoiceStateUpdatedHandler: Option<EventArgs.VoiceStateUpdateEventArgs -> unit>
        GuildAvailableHandler: Option<EventArgs.GuildCreateEventArgs -> unit>
        InviteCreatedHandler: Option<EventArgs.InviteCreateEventArgs -> unit>
        InviteDeletedHandler: Option<EventArgs.InviteDeleteEventArgs -> unit>
        Scheduler: Option<DiscordClient -> ref<bool>>
    }

let empty: BotModule =
    {
        MessageCreateEventHandleExclude = None
        MessageCreateEventHandle = None
        MessageDeletedHandler = None
        ComponentInteractionCreateHandle = None
        ModalSubmit = None
        GuildRoleDeletedHandler = None
        GuildMemberAddedHandler = None
        GuildMemberRemovedHandler = None
        GuildMemberUpdatedHandler = None
        MessageReactionAddedHandler = None
        MessageReactionRemoved = None
        VoiceStateUpdatedHandler = None
        GuildAvailableHandler = None
        InviteCreatedHandler = None
        InviteDeletedHandler = None
        Scheduler = None
    }

let bindToClientsEvents initCommandParser startCommandParser cmd appsHubResp (client: DiscordClient) (botModules: BotModule []) =
    let logger = client.Logger

    let schedulers =
        botModules
        |> Array.choose (fun x ->
            x.Scheduler
        )

    client.add_GuildDownloadCompleted (Emzi0767.Utilities.AsyncEventHandler (fun client e ->
        logger.LogInformation(botEventId, "Guild download completed.")

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
        logger.LogInformation(botEventId, sprintf "Guild \"%s\" (%d) available" e.Guild.Name e.Guild.Id)

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
            |> initCommandParser
        startCommandParser pcommands

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

    let componentInteractionCreatedHandlers =
        botModules
        |> Array.choose (fun x ->
            x.ComponentInteractionCreateHandle
        )
        |> List.ofArray
    client.add_ComponentInteractionCreated (Emzi0767.Utilities.AsyncEventHandler (fun client e ->
        let isHandled =
            componentInteractionCreatedHandlers
            |> List.exactlyFold
                (fun st f ->
                    let st = f (client, e)
                    st, st
                )
                false

        if not isHandled then
            appsHubResp client e

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
