module Shared.MessageTemplate
open FParsec
open FsharpMyExtension
open FsharpMyExtension.Either

open Types
open DiscordMessage.Parser

type 'a Parser = Parser<'a, unit>

/// Part of message template
type Part =
    | Text of string
    | UserName
    | UserMention

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Part =
    let userNameName = "userName"
    let userMentionName = "userMention"

    let toString = function
        | Text x -> x
        | UserName -> sprintf "<@%s>" userNameName
        | UserMention -> sprintf "<@%s>" userMentionName

    let Parser: Part Parser =
        let praw = many1Satisfy ((<>) '<')

        choice [
            praw |>> Text
            puserMentionTargetStr userNameName >>% UserName
            puserMentionTargetStr userMentionName >>% UserMention
            pstring "<" |>> Text
        ]

    let substitute userMention userName = function
        | Text x -> x
        | UserMention -> userMention
        | UserName -> userName

type MessageRaw = string

type Message = Part list

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Message =
    let parser: Message Parser =
        many Part.Parser

    let parse message =
        FParsecExt.runEither parser message

    let toString (template: Message): MessageRaw =
        template |> List.map Part.toString |> System.String.Concat

    let substitute targetUserMention targetUserUsername (message: Message) =
        message
        |> List.map (Part.substitute targetUserMention targetUserUsername)
        |> System.String.Concat
