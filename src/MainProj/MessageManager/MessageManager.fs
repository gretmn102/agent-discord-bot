module MessageManager
open FsharpMyExtension
open FsharpMyExtension.Either
open DSharpPlus

open Types

type MessageUrl = string

type Request =
    | SendMessage of Either<string, ChannelId * Entities.DiscordMessageBuilder>
    | GetMessage of MessagePath

module Parser =
    open FParsec
    open DSharpPlus.Net.Serialization

    open DiscordMessage.Parser

    type 'Result Parser = Primitives.Parser<'Result, unit>

    let psendMessage: _ Parser =
        let pcodeBlock: _ Parser =
            between (skipString "```" .>> skipManySatisfy ((<>) '\n') .>> skipChar '\n')
                (skipString "```")
                (manyStrings (
                    many1Satisfy ((<>) '`')
                    <|> (notFollowedByString "```" >>. preturn "`"))
                )

        skipStringCI "sendMessage" >>. spaces
        >>. pipe2
                (pchannelMention <|> puint64 .>> spaces)
                (pcodeBlock <|> manySatisfy (fun _ -> true))
                (fun channelId str ->
                    try
                        let raw = Newtonsoft.Json.Linq.JObject.Parse(str)
                        let msg = raw.ToDiscordObject<Entities.DiscordMessageBuilder>()
                        Right(channelId, msg)
                    with e ->
                        Left e.Message)

    let pgetMessage: _ Parser =
        skipStringCI "getMessage" >>. spaces
        >>. pipe3
                (skipString "https://discord.com/channels/" >>. puint64 .>> pchar '/')
                (puint64 .>> pchar '/')
                puint64
                (fun guildId channelId messageId ->
                    {
                        GuildId = guildId
                        ChannelId = channelId
                        MessageId = messageId
                    }
                )

    let start: _ Parser =
        choice [
            psendMessage |>> SendMessage
            pgetMessage |>> GetMessage
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


type Msg = EventArgs.MessageCreateEventArgs * Request

type State = unit

let reduce ((e, msg): Msg) (state: State): State =
    match msg with
    | SendMessage x ->
        let currentMember = await (e.Guild.GetMemberAsync(e.Author.Id))
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
                    awaiti (targetChannel.SendMessageAsync msg)

                    awaiti (replyMessage.ModifyAsync(Entities.Optional("Done")))
            | Left x ->
                awaiti (replyMessage.ModifyAsync(Entities.Optional(sprintf "```\n%s\n```" x)))
        else
            let msg =
                sprintf "%s you don't have administration permission for this command" e.Author.Mention
            awaiti (replyMessage.ModifyAsync(Entities.Optional msg))

        state

    | GetMessage messagePath ->
        let currentMember = await (e.Guild.GetMemberAsync(e.Author.Id))
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

let exec e msg =
    m.Post (e, msg)
