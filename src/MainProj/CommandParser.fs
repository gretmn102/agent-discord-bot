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

type Act =
    | Take
    | Fairytail
    | Catail
    | Bully
    | Admire
    | Battery

type Cmd =
    | Act of Act * UserId option
    | SomeCyoa
    | SomeGirlsQuiz
    | SomeQuiz
    | Unknown
    | Pass

let prefix = pchar '.'

let pcommand =
    let cmd =
        choice [
            skipString "take" >>% Take
            skipString "fairytail" >>% Fairytail
            skipString "catail" >>% Catail
            skipString "bully" >>. optional (skipString "ing") >>% Bully
            skipString "admire" >>% Admire
            skipString "battery" >>% Battery
        ]
    choice [
        cmd .>> spaces .>>. opt puserMention |>> Act
        stringReturn "someGirlsQuiz" SomeGirlsQuiz
        stringReturn "cyoa" SomeCyoa
        stringReturn "quiz" SomeQuiz
    ]

let start botId str =
    let p =
        (attempt (puserMentionTarget botId) >>. spaces >>. (optional prefix >>. pcommand <|>% Unknown))
        <|> ((prefix >>? pcommand) <|>% Pass)
    match run p str with
    | Success(x, _, _) -> Right x
    | Failure(x, _, _) -> Left x
