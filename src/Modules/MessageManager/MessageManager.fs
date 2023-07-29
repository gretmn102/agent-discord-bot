module MessageManager
open FsharpMyExtension
open FsharpMyExtension.Either
open DSharpPlus
open DSharpPlus.Net.Serialization
open DiscordBotExtensions
open DiscordBotExtensions.Types
open DiscordBotExtensions.Extensions
open DiscordBotExtensions.DiscordMessage

type MessageUrl = string

type Request =
    | SendMessage of ChannelId * DataOrUrl option
    | GetMessage of MessagePath
    | EditMessage of MessagePath * DataOrUrl option
    | SwitchBotReactions of MessagePath * DiscordMessage.UnicodeOrCustomEmoji list

module Parser =
    open FParsec

    open DiscordMessage.Parser

    type 'Result Parser = Primitives.Parser<'Result, unit>

    let pdiscordMessage: _ Parser =
        DataOrUrl.Parser.parser

    let psendMessage: _ Parser =
        skipStringCI "sendMessage" >>. spaces
        >>. tuple2
                (pchannelMention <|> puint64 .>> spaces)
                (opt pdiscordMessage)

    let pgetMessage: _ Parser =
        skipStringCI "getMessage" >>. spaces
        >>. pmessagePath

    let peditMessage: _ Parser =
        skipStringCI "editMessage" >>. spaces
        >>. tuple2
                (pmessagePath .>> spaces)
                (opt pdiscordMessage)

    let pswitchBotReactions: _ Parser =
        skipStringCI "switchBotReactions" >>. spaces
        >>. tuple2
                (pmessagePath .>> spaces)
                (many1 (pemoji .>> spaces))

    let start f: _ Parser =
        choice [
            psendMessage |>> SendMessage
            pgetMessage |>> GetMessage
            peditMessage |>> EditMessage
            pswitchBotReactions |>> SwitchBotReactions
        ]
        >>= fun msg ->
            preturn (fun x -> f x msg)

open Newtonsoft.Json

type RestChannelMessageEditPayload = {
    [<JsonProperty("content", NullValueHandling = NullValueHandling.Include)>]
    Content: string

    [<JsonProperty("embeds", NullValueHandling = NullValueHandling.Ignore)>]
    Embeds: Entities.DiscordEmbed System.Collections.Generic.IReadOnlyList

    // [<JsonProperty("allowed_mentions", NullValueHandling = NullValueHandling.Ignore)>]
    // Mentions: DiscordMentions

    [<JsonProperty("components", NullValueHandling = NullValueHandling.Ignore)>]
    Components: System.Collections.Generic.IReadOnlyCollection<Entities.DiscordActionRowComponent>

    [<JsonProperty("flags", NullValueHandling = NullValueHandling.Ignore)>]
    Flags: System.Nullable<MessageFlags>

    [<JsonProperty("attachments", NullValueHandling = NullValueHandling.Ignore)>]
    Attachments: Entities.DiscordAttachment System.Collections.Generic.IReadOnlyList
}


type Msg = DiscordClient * EventArgs.MessageCreateEventArgs * Request

type State = unit

let reduce ((client, e, msg): Msg) (state: State): State =
    let send msg =
        let embed = Entities.DiscordEmbedBuilder()
        embed.Color <- Entities.Optional.FromValue(DiscordEmbed.backgroundColorDarkTheme)
        embed.Description <- msg

        let b = Entities.DiscordMessageBuilder()
        b.Embed <- embed.Build()

        awaiti <| e.Channel.SendMessageAsync b

    let checkAccess (guildMember: Entities.DiscordMember) next =
        if guildMember.Permissions &&& Permissions.Administrator = Permissions.Administrator then
            next ()
        else
            sprintf "%s you don't have administration permission for this command" e.Author.Mention
            |> send

            state

    let getJson json next =
        match DataOrUrl.getOrAttachment e.Message json with
        | Ok x -> next x
        | Error errMsg ->
            send errMsg
            state

    let deserializeDiscordMessage rawJson next =
        try
            let raw = Newtonsoft.Json.Linq.JObject.Parse(rawJson)
            let msg = raw.ToDiscordObject<Entities.DiscordMessageBuilder>()
            next msg
        with e ->
            sprintf "```\n%s\n```" e.Message
            |> send

            state

    let getChannel (channelId: ChannelId) next =
        match e.Guild.GetChannel channelId with
        | null ->
            sprintf "%d channel not found" channelId
            |> send

            state
        | channel ->
            next channel

    let getMessage (channel: Entities.DiscordChannel) (messageId: MessageId) next =
        match await (channel.GetMessageAsync messageId) with
        | null ->
            sprintf "Not found %d message" messageId
            |> send

            state
        | msg ->
            next msg

    match msg with
    | SendMessage (targetChannelId, res) ->
        awaiti <| e.Channel.TriggerTypingAsync()

        let currentMember = DiscordGuild.getMember e.Author e.Guild

        checkAccess currentMember <| fun () ->
        getJson res <| fun rawJson ->
        deserializeDiscordMessage rawJson <| fun msg ->
        getChannel targetChannelId <| fun targetChannel ->

        try
            let newMessage = await (targetChannel.SendMessageAsync msg)

            let msg =
                sprintf "%s\nDone!"
                    (MessagePath.OfDiscordMessage newMessage).ToDiscordPath

            send msg
        with e ->
            sprintf "```\n%s\n```" e.Message
            |> send

        state

    | EditMessage(messagePath, res) ->
        let checkMessageBelongToBot (message: Entities.DiscordMessage) next =
            if message.Author.Id = client.CurrentUser.Id then
                next ()
            else
                "this message does not belong to a bot"
                |> send

                state

        awaiti <| e.Channel.TriggerTypingAsync()

        let currentMember = DiscordGuild.getMember e.Author e.Guild

        checkAccess currentMember <| fun () ->
        getJson res <| fun rawJson ->
        deserializeDiscordMessage rawJson <| fun newMessage ->
        getChannel messagePath.ChannelId <| fun channel ->
        getMessage channel messagePath.MessageId <| fun oldMessage ->
        checkMessageBelongToBot oldMessage <| fun () ->

        awaiti <| oldMessage.ModifyAsync(newMessage)
        send "Done"

        state

    | GetMessage messagePath ->
        let checkMessageBelongToCurrentGuild (messagePath: MessagePath) next =
            if e.Guild.Id = messagePath.GuildId then
                next ()
            else
                sprintf "You can specify a message only for the current guild"
                |> send

                state

        awaiti <| e.Channel.TriggerTypingAsync()

        let currentMember = DiscordGuild.getMember e.Author e.Guild

        checkAccess currentMember <| fun () ->
        checkMessageBelongToCurrentGuild messagePath <| fun () ->
        getChannel messagePath.ChannelId <| fun channel ->
        getMessage channel messagePath.MessageId <| fun msg ->

        let msg =
            {
                Content = msg.Content
                Embeds = msg.Embeds
                Components = msg.Components
                Flags = msg.Flags
                Attachments = msg.Attachments
            }
            |> DiscordJson.SerializeObject

        let resultMsg =
            sprintf "```\n%s\n```" msg

        if resultMsg.Length > 4096 then
            let b = Entities.DiscordMessageBuilder()
            let fileName = "message.txt"
            let bytes = System.Text.Encoding.UTF8.GetBytes msg
            use m = new System.IO.MemoryStream(bytes)
            b.AddFile(fileName, m) |> ignore
            awaiti <| e.Channel.SendMessageAsync b
        else
            send resultMsg

        state

    | SwitchBotReactions(messagePath, emojis) ->
        let currentMember = DiscordGuild.getMember e.Author e.Guild
        let replyMessage =
            await (e.Channel.SendMessageAsync("Processing..."))
        if (currentMember.Permissions &&& Permissions.Administrator = Permissions.Administrator) then
            if e.Guild.Id = messagePath.GuildId then
                match e.Guild.GetChannel messagePath.ChannelId with
                | null ->
                    let msg =
                        sprintf "Not found %d channel" messagePath.ChannelId

                    awaiti (replyMessage.ModifyAsync(Entities.Optional msg))
                | channel ->
                    match await (channel.GetMessageAsync messagePath.MessageId) with
                    | null ->
                        let msg =
                            sprintf "Not found %d message" messagePath.MessageId

                        awaiti (replyMessage.ModifyAsync(Entities.Optional msg))

                    | msg ->
                        emojis
                        |> List.iter (fun emoji ->
                            let emoji =
                                match emoji with
                                | DiscordMessage.CustomEmoji x ->
                                    match Entities.DiscordEmoji.TryFromGuildEmote(client, x.Id) with
                                    | true, emoji -> Some emoji
                                    | false, _ -> None
                                | DiscordMessage.UnicodeEmoji x ->
                                    match Entities.DiscordEmoji.TryFromUnicode(client, x) with
                                    | true, emoji -> Some emoji
                                    | false, _ -> None

                            emoji
                            |> Option.iter (fun emoji ->
                                let reactions = await (msg.GetReactionsAsync(emoji))
                                if reactions |> Seq.exists (fun x -> x.Id = client.CurrentUser.Id) then
                                    awaiti <| msg.DeleteOwnReactionAsync emoji
                                else
                                    awaiti <| msg.CreateReactionAsync emoji
                            )
                        )

                        let msg = "Done"

                        awaiti (replyMessage.ModifyAsync(Entities.Optional msg))
            else
                let msg =
                    sprintf "You can specify a message only for the current guild"

                awaiti (replyMessage.ModifyAsync(Entities.Optional msg))
        else
            let msg =
                sprintf "%s you don't have administration permission for this command" e.Author.Mention
            awaiti (replyMessage.ModifyAsync(Entities.Optional msg))

        state

let create () =
    let m =
        let init = ()

        MailboxProcessor.Start (fun mail ->
            let rec loop (state: State) =
                async {
                    let! (msg: Msg) = mail.Receive()
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

    { BotModule.empty with
        MessageCreateEventHandleExclude =
            let exec: _ Parser.Parser =
                Parser.start (fun (client: DiscordClient, e: EventArgs.MessageCreateEventArgs) msg ->
                    m.Post (client, e, msg)
                )
            Some exec
    }
