module Calc.Main
open DSharpPlus

open Types

type Request = float

module Parser =
    open FParsec

    type 'a Parser = Parser<'a, unit>

    let pexpr: _ Parser =
        Core.Parser.pexpr

    let start: Request Parser =
        pstring "calc" >>. spaces >>. pexpr |>> Core.calc

let exec (e: EventArgs.MessageCreateEventArgs) (r: Request) =
    awaiti <| e.Channel.TriggerTypingAsync()

    let floatToString (x: float) =
        if x % 1.0 = 0.0 then
            sprintf "%A" (int x)
        else
            sprintf "%A" x

    awaiti <| e.Channel.SendMessageAsync (floatToString r)
