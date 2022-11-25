module NumberToWords.Main
open FsharpMyExtension
open DSharpPlus

open Types
open Extensions

type Request =
    | NumberToWords of bigint

module Parser =
    open FParsec

    open DiscordMessage.Parser

    type 'Result Parser = Primitives.Parser<'Result, unit>

    let start f: _ Parser =
        pstringCI "numberToWords" >>. spaces >>. FParsecExt.pbigint |>> NumberToWords
        >>= fun msg ->
            preturn (fun x -> f x msg)

type Req =
    | MessageCreateEventHandler of e: EventArgs.MessageCreateEventArgs * request: Request

let reduce req state =
    match req with
    | MessageCreateEventHandler(e, req) ->
        match req with
        | NumberToWords num ->
            let b = Entities.DiscordEmbedBuilder()
            b.Description <-
                try
                    Core.toNumName num
                with e ->
                    e.Message
            b.Color <- Entities.Optional.FromValue(DiscordEmbed.backgroundColorDarkTheme)

            awaiti <| e.Channel.SendMessageAsync(b.Build())

let exec: MessageCreateEventHandler Parser.Parser =
    Parser.start (fun (client: DiscordClient, e: EventArgs.MessageCreateEventArgs) msg ->
        reduce (MessageCreateEventHandler(e, msg)) ()
    )
