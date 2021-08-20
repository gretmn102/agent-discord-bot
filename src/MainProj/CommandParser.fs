module CommandParser
open FsharpMyExtension
open FsharpMyExtension.Either
open Types
open FParsec

type 'a Parser = Parser<'a, unit>
let puserMention : _ Parser =
    skipString "<@" >>. optional (skipChar '!') >>. puint64 .>> skipChar '>'
let puserMentionTarget (userId:UserId) : _ Parser =
    skipString "<@" >>. optional (skipChar '!') >>. skipString (string userId) >>. skipChar '>'

type Cmd =
    | Take of UserId option
    | Unknown
    | Pass

let prefix = pchar '.'

let pcommand =
    pstring "take" >>. spaces >>. opt puserMention |>> Take

let start botId str =
    let p =
        (puserMentionTarget botId >>. spaces >>. (optional prefix >>. pcommand <|>% Unknown))
        <|> ((prefix >>? pcommand) <|>% Pass)
    match run p str with
    | Success(x, _, _) -> Right x
    | Failure(x, _, _) -> Left x
