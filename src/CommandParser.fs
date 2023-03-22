module CommandParser
open FsharpMyExtension
open FsharpMyExtension.Either
open FParsec

open Types
open DiscordMessage
open DiscordMessage.Parser

type 'a Parser = Parser<'a, unit>

type Cmd =
    | Unknown
    | Pass

    | MessageCreateEventHandler of Shared.BotModule.MessageCreateEventHandler

let prefix = pchar '.'

let initCommandParser (commands: Parser<Shared.BotModule.MessageCreateEventHandler> seq): _ Parser =
    let pmessageCreateEventHandler = choice commands

    choice [
        pmessageCreateEventHandler |>> MessageCreateEventHandler
    ]

let start (pcommand: _ Parser) botId =
    let p =
        (attempt (puserMentionTarget botId) >>. spaces >>. (optional prefix >>. pcommand <|>% Unknown))
        <|> ((prefix >>? pcommand) <|>% Pass)

    FParsecExt.runEither p
