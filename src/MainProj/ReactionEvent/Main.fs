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

    let start: _ Parser =
        choice [
            psetReactionEvent |>> SetReactionEvent
        ]

[<Struct>]
type Changed = Added | Removed

type ChangedEvent =
    | AddedEvent of EventArgs.MessageReactionAddEventArgs
    | RemovedEvent of EventArgs.MessageReactionRemoveEventArgs

type Msg =
    | GuildReactionHandle of ChangedEvent
    | Request of EventArgs.MessageCreateEventArgs * Request

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
                    Key = ({
                        ChannelId = e.Channel.Id
                        MessageId = e.Message.Id
                        EmojiId = e.Emoji.Id
                        EmojiName = e.Emoji.Name
                    } : Model.ReactionEvents.Key)
                |}
            | RemovedEvent e ->
                {|
                    Changed = Removed
                    Guild = e.Guild
                    User = e.User
                    Key = ({
                        ChannelId = e.Channel.Id
                        MessageId = e.Message.Id
                        EmojiId = e.Emoji.Id
                        EmojiName = e.Emoji.Name
                    } : Model.ReactionEvents.Key)
                |}

        match Map.tryFind x.Guild.Id state.ReactionEvents with
        | None -> ()
        | Some reactionEvents ->
            match Map.tryFind x.Key reactionEvents with
            | None -> ()
            | Some data ->
                let guildMember = await (x.Guild.GetMemberAsync(x.User.Id))
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
                            (grantOrRevokeRole role).GetAwaiter().GetResult()
                        with e ->
                            ()
                )

        state
    | Request(e, req) ->
        match req with
        | SetReactionEvent(messagePath, emoji, roleIds) ->
            let currentMember = await (e.Guild.GetMemberAsync e.Author.Id)
            let replyMessage =
                await (e.Channel.SendMessageAsync("Processing..."))

            if currentMember.Permissions &&& Permissions.Administrator = Permissions.Administrator then
                let key: Model.ReactionEvents.Key = {
                    ChannelId = messagePath.ChannelId
                    MessageId = messagePath.MessageId
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
                        (e.Guild.Id, key.ChannelId, key.MessageId, key.EmojiId, key.EmojiName, Set.ofList roleIds)

                let state =
                    { state with
                        ReactionEvents =
                            state.ReactionEvents
                            |> Map.addOrModWith
                                e.Guild.Id
                                (fun () ->
                                    Map.add key data Map.empty
                                )
                                (fun m ->
                                    Map.add key data m
                                )
                    }

                let msg = "Done"
                awaiti (replyMessage.ModifyAsync(Entities.Optional msg))

                state
            else
                let msg = "You don't have administration permission"
                awaiti (replyMessage.ModifyAsync(Entities.Optional msg))

                state

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

let exec e msg =
    m.Post (Request (e, msg))
