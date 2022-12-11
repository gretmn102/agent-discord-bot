module CommandParser
open FsharpMyExtension
open FsharpMyExtension.Either
open FParsec

open Types
open DiscordMessage
open DiscordMessage.Parser

type 'a Parser = Parser<'a, unit>

type Cmd =
    | Cyoa of AppsHub.Hub.CyoaT
    | SomeQuiz
    | Unknown
    | Pass

    | MessageCreateEventHandler of MessageCreateEventHandler

let prefix = pchar '.'

let initCommandParser (commands: Parser<MessageCreateEventHandler> seq): _ Parser =
    let pmessageCreateEventHandler = choice commands

    choice [
        stringReturn "someGirlsQuiz" (Cyoa AppsHub.Hub.SomeGirlsQuiz)
        stringReturn "cyoa" (Cyoa AppsHub.Hub.SomeCyoa)
        stringReturn "quizWithMultiChoices" (Cyoa AppsHub.Hub.QuizWithMultiChoices)
        stringReturn "quizPizza" (Cyoa AppsHub.Hub.QuizPizza)
        stringReturn "quiz" SomeQuiz

        pmessageCreateEventHandler |>> MessageCreateEventHandler
    ]

let start (pcommand: _ Parser) botId =
    let p =
        (attempt (puserMentionTarget botId) >>. spaces >>. (optional prefix >>. pcommand <|>% Unknown))
        <|> ((prefix >>? pcommand) <|>% Pass)

    FParsecExt.runEither p
