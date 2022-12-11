module Shared.BotModule
open DSharpPlus
open FParsec

open Types

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
