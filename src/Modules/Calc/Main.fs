module Calc.Main
open DSharpPlus

open Shared
open Types

type Request = float

module Parser =
    open FParsec

    type 'a Parser = Parser<'a, unit>

    let pexpr: _ Parser =
        Core.Parser.pexpr

    let start f: _ Parser =
        pstring "calc" >>. spaces >>. pexpr |>> Core.calc
        >>= fun msg ->
            preturn (fun x -> f x msg)

let reduce (e: EventArgs.MessageCreateEventArgs) (r: Request) =
    awaiti <| e.Channel.TriggerTypingAsync()

    let floatToString (x: float) =
        if x % 1.0 = 0.0 then
            sprintf "%A" (int x)
        else
            sprintf "%A" x

    awaiti <| e.Channel.SendMessageAsync (floatToString r)

let create () =
    { BotModule.empty with
        MessageCreateEventHandleExclude =
            let exec: MessageCreateEventHandler Parser.Parser =
                Parser.start (fun (client: DiscordClient, e: EventArgs.MessageCreateEventArgs) msg ->
                    reduce e msg
                )
            Some exec
    }
