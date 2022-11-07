module Roll.Main
open DSharpPlus

open Types

type RollSetting =
    {
        DicesCount: int
        DiceMaxValue: int
        Description: string option
    }

type Request =
    | Roll of RollSetting

module Parser =
    open FParsec

    open DiscordMessage.Parser

    type 'Result Parser = Primitives.Parser<'Result, unit>

    module CommandNames =
        let roll = "roll"

    let start f: _ Parser =
        let proll =
            let pdice =
                tuple2
                    (opt (pint32 .>> spaces))
                    (pchar 'd' >>. pint32)

            skipStringCI CommandNames.roll .>> spaces
            >>. pipe2
                    (pdice .>> spaces)
                    (opt (many1Satisfy (fun _ -> true)))
                    (fun (dicesCount, diceMaxValue) description ->
                        {
                            DicesCount = dicesCount |> Option.defaultValue 1
                            DiceMaxValue = diceMaxValue
                            Description = description
                        }
                    )

        choice [
            proll |>> Roll
        ]
        >>= fun msg ->
            preturn (fun x -> f x msg)

let r = System.Random ()

let roll (setting: RollSetting) =

    let throwResultString =
        let throw max =
            r.Next(1, max + 1)

        if setting.DicesCount > 1 then
            let throwResults =
                List.init setting.DicesCount (fun _ ->
                    throw setting.DiceMaxValue
                )
            let throwResult = List.sum throwResults
            let throwResults =
                throwResults
                |> List.map string
                |> String.concat " + "
            sprintf "%A = %d" throwResults throwResult
        else
            sprintf "%d" (throw setting.DiceMaxValue)

    match setting.Description with
    | None ->
        sprintf "Итог броска — %s." throwResultString
    | Some description ->
        sprintf "%s %s." description throwResultString

let reduce (e: EventArgs.MessageCreateEventArgs) (req: Request) =
    match req with
    | Roll setting ->
        let res = roll setting

        awaiti <| e.Channel.SendMessageAsync(res)

let exec: MessageCreateEventHandler Parser.Parser =
    Parser.start (fun (client: DiscordClient, e: EventArgs.MessageCreateEventArgs) msg ->
        reduce e msg
    )
