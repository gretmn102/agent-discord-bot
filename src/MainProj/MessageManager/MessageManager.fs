module MessageManager
open FsharpMyExtension
open FsharpMyExtension.Either
open DSharpPlus

open Types

type MessageUrl = string

type Request =
    | SendMessage of Either<string, ChannelId * Entities.DiscordMessageBuilder>
    | GetMessage of MessagePath
    | EditMessage of Either<string, MessagePath * Entities.DiscordMessageBuilder>
    | SwitchBotReactions of MessagePath * DiscordMessage.UnicodeOrCustomEmoji list

module Parser =
    open FParsec
    open DSharpPlus.Net.Serialization

    open DiscordMessage.Parser

    type 'Result Parser = Primitives.Parser<'Result, unit>

    let pmessage: _ Parser =
        pcodeBlock <|> manySatisfy (fun _ -> true)

    let psendMessage: _ Parser =
        skipStringCI "sendMessage" >>. spaces
        >>. pipe2
                (pchannelMention <|> puint64 .>> spaces)
                pmessage
                (fun channelId str ->
                    try
                        let raw = Newtonsoft.Json.Linq.JObject.Parse(str)
                        let msg = raw.ToDiscordObject<Entities.DiscordMessageBuilder>()
                        Right(channelId, msg)
                    with e ->
                        Left e.Message)

    let pgetMessage: _ Parser =
        skipStringCI "getMessage" >>. spaces
        >>. pmessagePath

    let peditMessage: _ Parser =
        skipStringCI "editMessage" >>. spaces
        >>. pipe2
                (pmessagePath .>> spaces)
                pmessage
                (fun messagePath str ->
                    try
                        let raw = Newtonsoft.Json.Linq.JObject.Parse(str)
                        let msg = raw.ToDiscordObject<Entities.DiscordMessageBuilder>()
                        Right(messagePath, msg)
                    with e ->
                        Left e.Message)

    let pswitchBotReactions: _ Parser =
        skipStringCI "switchBotReactions" >>. spaces
        >>. tuple2
                (pmessagePath .>> spaces)
                (many1 (pemoji .>> spaces))

    let start: _ Parser =
        choice [
            psendMessage |>> SendMessage
            pgetMessage |>> GetMessage
            peditMessage |>> EditMessage
            pswitchBotReactions |>> SwitchBotReactions
        ]


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
    match msg with
    | SendMessage x ->
        let currentMember = getGuildMember e.Guild e.Author
        let replyMessage =
            await (e.Channel.SendMessageAsync("Processing..."))

        if (currentMember.Permissions &&& Permissions.Administrator = Permissions.Administrator) then
            match x with
            | Right (targetChannelId, msg) ->
                match e.Guild.GetChannel targetChannelId with
                | null ->
                    let msg =
                        sprintf "%d channel not found" targetChannelId
                    awaiti (replyMessage.ModifyAsync(Entities.Optional msg))
                | targetChannel ->
                    try
                        let newMessage = await (targetChannel.SendMessageAsync msg)

                        let msg =
                            sprintf "%s\nDone!"
                                (MessagePath.OfDiscordMessage newMessage).ToDiscordPath

                        awaiti (replyMessage.ModifyAsync(Entities.Optional msg))
                    with e ->
                        let msg =
                            sprintf "```\n%s\n```" e.Message
                        awaiti (replyMessage.ModifyAsync(Entities.Optional(msg)))

            | Left x ->
                awaiti (replyMessage.ModifyAsync(Entities.Optional(sprintf "```\n%s\n```" x)))
        else
            let msg =
                sprintf "%s you don't have administration permission for this command" e.Author.Mention
            awaiti (replyMessage.ModifyAsync(Entities.Optional msg))

        state

    | GetMessage messagePath ->
        let currentMember = getGuildMember e.Guild e.Author
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
                        let msg =
                            {
                                Content = msg.Content
                                Embeds = msg.Embeds
                                Components = msg.Components
                                Flags = msg.Flags
                                Attachments = msg.Attachments
                            }

                        let msg =
                            sprintf "```\n%s\n```" (Net.Serialization.DiscordJson.SerializeObject(msg))

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

    | EditMessage x ->
        let currentMember = getGuildMember e.Guild e.Author
        let replyMessage =
            await (e.Channel.SendMessageAsync("Processing..."))
        if (currentMember.Permissions &&& Permissions.Administrator = Permissions.Administrator) then
            match x with
            | Right (messagePath, msgBuilder) ->
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
                            if msg.Author.Id = client.CurrentUser.Id then
                                awaiti (msg.ModifyAsync(msgBuilder))

                                awaiti (replyMessage.ModifyAsync(Entities.Optional "Done"))
                            else
                                let msg =
                                    "this message does not belong to a bot"
                                awaiti (replyMessage.ModifyAsync(Entities.Optional msg))
                else
                    let msg =
                        sprintf "You can specify a message only for the current guild"

                    awaiti (replyMessage.ModifyAsync(Entities.Optional msg))
            | Left errMsg ->
                    let msg =
                        sprintf "```\n%s\n```" errMsg

                    awaiti (replyMessage.ModifyAsync(Entities.Optional msg))
        else
            let msg =
                sprintf "%s you don't have administration permission for this command" e.Author.Mention
            awaiti (replyMessage.ModifyAsync(Entities.Optional msg))

        state

    | SwitchBotReactions(messagePath, emojis) ->
        let currentMember = getGuildMember e.Guild e.Author
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
                                    (msg.DeleteOwnReactionAsync emoji).GetAwaiter().GetResult()
                                else
                                    (msg.CreateReactionAsync emoji).GetAwaiter().GetResult()
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

let exec client e msg =
    m.Post (client, e, msg)
