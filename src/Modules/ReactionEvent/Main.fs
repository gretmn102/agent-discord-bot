module ReactionEvent.Main
open FsharpMyExtension
open FsharpMyExtension.Either
open DSharpPlus

open Types

type Request =
    | SetReactionEvent of MessagePath * DiscordMessage.UnicodeOrCustomEmoji * RoleId list

module Parser =
    open FParsec

    open DiscordMessage.Parser

    type 'Result Parser = Primitives.Parser<'Result, unit>

    let psetReactionEvent: _ Parser =
        skipStringCI "setReactionEvent" >>. spaces
        >>. tuple3
                (pmessagePath .>> spaces)
                (pemoji .>> spaces)
                (many (pmentionRole <|> puint64 .>> spaces))

    let start f: _ Parser =
        choice [
            psetReactionEvent |>> SetReactionEvent
        ]
        >>= fun msg ->
            preturn (fun x -> f x msg)

[<Struct>]
type Changed = Added | Removed

type ChangedEvent =
    | AddedEvent of EventArgs.MessageReactionAddEventArgs
    | RemovedEvent of EventArgs.MessageReactionRemoveEventArgs

type Msg =
    | GuildReactionHandle of ChangedEvent
    | Request of EventArgs.MessageCreateEventArgs * Request
    | MessageDeletedHandle of EventArgs.MessageDeleteEventArgs

type State = {
    ReactionEvents: Model.ReactionEvents.GuildReactionEvent
}

let reduce (msg: Msg) (state: State) =
    match msg with
    | GuildReactionHandle x ->
        let x =
            match x with
            | AddedEvent e ->
                {|
                    Changed = Added
                    Guild = e.Guild
                    User = e.User
                    MessagePath = ({
                        ChannelId = e.Channel.Id
                        MessageId = e.Message.Id
                    } : Model.ReactionEvents.MessagePath)
                    Emoji = ({
                        EmojiId = e.Emoji.Id
                        EmojiName = e.Emoji.Name
                    } : Model.ReactionEvents.Emoji)
                |}
            | RemovedEvent e ->
                {|
                    Changed = Removed
                    Guild = e.Guild
                    User = e.User
                    MessagePath = ({
                        ChannelId = e.Channel.Id
                        MessageId = e.Message.Id
                    } : Model.ReactionEvents.MessagePath)
                    Emoji = ({
                        EmojiId = e.Emoji.Id
                        EmojiName = e.Emoji.Name
                    } : Model.ReactionEvents.Emoji)
                |}

        match Map.tryFind x.Guild.Id state.ReactionEvents with
        | None -> ()
        | Some reactionEvents ->
            match Map.tryFind x.MessagePath reactionEvents with
            | None -> ()
            | Some data ->
                match Map.tryFind x.Emoji data with
                | None -> ()
                | Some data ->
                    let guildMember = getGuildMember x.Guild x.User
                    let grantOrRevokeRole =
                        match x.Changed with
                        | Added -> guildMember.GrantRoleAsync
                        | Removed -> guildMember.RevokeRoleAsync

                    data.RoleIds
                    |> Set.iter (fun roleId ->
                        match x.Guild.GetRole roleId with
                        | null -> ()
                        | role ->
                            try
                                awaiti <| grantOrRevokeRole role
                            with e ->
                                ()
                    )

        state
    | Request(e, req) ->
        match req with
        | SetReactionEvent(messagePath, emoji, roleIds) ->
            let currentMember = getGuildMember e.Guild e.Author
            let replyMessage =
                await (e.Channel.SendMessageAsync("Processing..."))

            if currentMember.Permissions &&& Permissions.Administrator = Permissions.Administrator then
                let messagePath: Model.ReactionEvents.MessagePath = {
                    ChannelId = messagePath.ChannelId
                    MessageId = messagePath.MessageId
                }

                let emoji: Model.ReactionEvents.Emoji = {
                    EmojiId =
                        match emoji with
                        | DiscordMessage.CustomEmoji e -> e.Id
                        | DiscordMessage.UnicodeEmoji _ -> 0UL
                    EmojiName =
                        match emoji with
                        | DiscordMessage.CustomEmoji e -> e.Name
                        | DiscordMessage.UnicodeEmoji name -> name
                }

                let data =
                    Model.ReactionEvents.insert
                        (e.Guild.Id, messagePath.ChannelId, messagePath.MessageId, emoji.EmojiId, emoji.EmojiName, Set.ofList roleIds)

                let state =
                    let add =
                        Map.addOrModWith
                            messagePath
                            (fun () -> Map.add emoji data Map.empty)
                            (Map.add emoji data)

                    { state with
                        ReactionEvents =
                            state.ReactionEvents
                            |> Map.addOrModWith
                                e.Guild.Id
                                (fun () -> add Map.empty)
                                add
                    }

                let msg = "Done"
                awaiti (replyMessage.ModifyAsync(Entities.Optional msg))

                state
            else
                let msg = "You don't have administration permission"
                awaiti (replyMessage.ModifyAsync(Entities.Optional msg))

                state
    | MessageDeletedHandle e ->
        match Map.tryFind e.Guild.Id state.ReactionEvents with
        | None -> state
        | Some reactionEvents ->
            let messagePath = ({
                ChannelId = e.Channel.Id
                MessageId = e.Message.Id
            } : Model.ReactionEvents.MessagePath)

            match Map.tryFind messagePath reactionEvents with
            | None -> state
            | Some data ->
                data
                |> Map.iter (fun _ data ->
                    Model.ReactionEvents.remove data
                )

                { state with
                    ReactionEvents =
                        let m = Map.remove messagePath reactionEvents
                        Map.add e.Guild.Id m state.ReactionEvents }

let m =
    let init = {
        ReactionEvents = Model.ReactionEvents.getAll ()
    }

    MailboxProcessor.Start (fun mail ->
        let rec loop (state: State) =
            async {
                let! msg = mail.Receive()
                let state =
                    try
                        reduce msg state
                    with e ->
                        printfn "%A" e
                        state

                return! loop state
            }
        loop init
    )

let handle e =
    m.Post (GuildReactionHandle e)

let exec: MessageCreateEventHandler Parser.Parser =
    Parser.start (fun (client: DiscordClient, e: EventArgs.MessageCreateEventArgs) msg ->
        m.Post (Request (e, msg))
    )

let messageDeletedHandle e =
    m.Post (MessageDeletedHandle e)
