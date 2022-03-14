module DiscordWebhook.Main
open FsharpMyExtension
open FsharpMyExtension.Either
open DSharpPlus

open Types

type Request =
    | Send of {| Username: string; AvatarUrl: string; Content: string |}

module Parser =
    open FParsec

    open DiscordMessage.Parser

    type 'a Parser = Parser<'a, unit>

    let purl: _ Parser =
        pipe2
            (pstring "http")
            (manySatisfy (not << System.Char.IsWhiteSpace))
            (sprintf "%s%s")

    let psend: _ Parser =
        skipStringCI "sendHook" .>> spaces
        >>. pipe3
                (pquote .>> spaces)
                (purl .>> spaces)
                (manySatisfy (fun _ -> true))
                (fun userName url msg ->
                    {| AvatarUrl = url; Username = userName; Content = msg |})

    let start: _ Parser =
        psend |>> Send

let reduce (e: EventArgs.MessageCreateEventArgs) msg =
    match msg with
    | Send opts ->
        let webhooks = await (e.Guild.GetWebhooksAsync())
        let webhook = webhooks.[0]
        let b = Entities.DiscordWebhookBuilder()

        b.WithUsername opts.Username |> ignore
        b.WithAvatarUrl opts.AvatarUrl |> ignore
        b.WithContent opts.Content |> ignore

        awaiti (webhook.ExecuteAsync b)

let exec e msg =
    reduce e msg
