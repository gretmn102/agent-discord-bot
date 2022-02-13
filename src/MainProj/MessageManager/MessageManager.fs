module MessageManager
open FsharpMyExtension
open FsharpMyExtension.Either
open DSharpPlus

open Types

type Request =
    | SendMessage of Either<string, ChannelId * Entities.DiscordMessageBuilder>

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

    let start: _ Parser =
        choice [
            psendMessage |>> SendMessage
        ]


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
