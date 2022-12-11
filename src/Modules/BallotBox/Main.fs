module BallotBox.Main
open DSharpPlus

open Shared
open Types

type Request =
    | BallotBox of description: string * choices: string list

module Parser =
    open FParsec

    open DiscordMessage.Parser

    type 'Result Parser = Primitives.Parser<'Result, unit>

    let pcreateBallotBox =
        pstring "ballotBox"
        >>. many1Satisfy ((=) ' ') >>. many1Satisfy ((<>) '\n') .>> spaces
        .>>. many1 (many1Satisfy ((<>) '\n') .>> spaces)

    let start f: _ Parser =
        pcreateBallotBox |>> BallotBox
        >>= fun msg ->
            preturn (fun x -> f x msg)

type Req =
    | MessageCreateEventHandler of client: DiscordClient * e: EventArgs.MessageCreateEventArgs * request: Request

let reduce req state appsHubInit =
    match req with
    | MessageCreateEventHandler (client, e, req) ->
        match req with
        | BallotBox(description, choices) ->
            appsHubInit (description, choices) client e

let create appsHubInit =
    { BotModule.empty with
        MessageCreateEventHandleExclude =
            let exec appsHubInit: _ Parser.Parser =
                Parser.start (fun (client: DiscordClient, e: EventArgs.MessageCreateEventArgs) msg ->
                    reduce (MessageCreateEventHandler(client, e, msg)) () appsHubInit
                )
            Some (exec appsHubInit)
    }
