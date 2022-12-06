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
    ReactionEvents: Model.Guilds
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
                    MessagePath = Model.MessagePath.create e.Channel.Id e.Message.Id
                    Emoji = Model.Emoji.create e.Emoji.Id e.Emoji.Name
                |}
            | RemovedEvent e ->
                {|
                    Changed = Removed
                    Guild = e.Guild
                    User = e.User
                    MessagePath = Model.MessagePath.create e.Channel.Id e.Message.Id
                    Emoji = Model.Emoji.create e.Emoji.Id e.Emoji.Name
                |}

        let id = Model.Id.create x.Guild.Id x.MessagePath x.Emoji
        match Model.Guilds.tryFindById id state.ReactionEvents with
        | None -> ()
        | Some item ->
            let guildMember = getGuildMember x.Guild x.User
            let grantOrRevokeRole =
                match x.Changed with
                | Added -> guildMember.GrantRoleAsync
                | Removed -> guildMember.RevokeRoleAsync

            item.Data.RoleIds
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
                let messagePath: Model.MessagePath = {
                    ChannelId = messagePath.ChannelId
                    MessageId = messagePath.MessageId
                }

                let emoji: Model.Emoji =
                    Model.Emoji.ofUnicodeOrCustomEmoji emoji

                let id =
                    Model.Id.create
                        e.Guild.Id
                        (Model.MessagePath.create messagePath.ChannelId messagePath.MessageId)
                        (Model.Emoji.create emoji.EmojiId emoji.EmojiName)

                let state =
                    { state with
                        ReactionEvents =
                            state.ReactionEvents
                            |> Model.Guilds.set
                                id
                                (fun data ->
                                    { data with
                                        RoleIds = Set.ofList roleIds
                                    }
                                )
                    }

                let msg = "Done"
                awaiti (replyMessage.ModifyAsync(Entities.Optional msg))

                state
            else
                let msg = "You don't have administration permission"
                awaiti (replyMessage.ModifyAsync(Entities.Optional msg))

                state

    | MessageDeletedHandle e ->
        let guildId = e.Guild.Id
        let messagePath =
            Model.MessagePath.create e.Channel.Id e.Message.Id

        { state with
            ReactionEvents =
                let _, db = Model.Guilds.removeByGuildIdAndMessagePath guildId messagePath state.ReactionEvents
                db
        }

let m =
    let init = {
        ReactionEvents = Model.Guilds.init "reactionEvents" Db.database
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
