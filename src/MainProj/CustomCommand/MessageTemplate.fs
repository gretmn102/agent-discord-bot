module CustomCommand.MessageTemplate
open FParsec
open FsharpMyExtension
open FsharpMyExtension.Either

open Types
open DiscordMessage.Parser

type 'a Parser = Parser<'a, unit>

[<Struct>]
type MentionType =
    | Name
    | Mention
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module MentionType =
    let toString (u: MentionType) =
        match u with
        | Name -> "Name"
        | Mention -> "Mention"

    let parse: MentionType Parser =
        choice [
            skipStringCI (toString Name) >>% Name
            skipStringCI (toString Mention) >>% Mention
        ]

[<Struct>]
type MentionName =
    | Author
    | Bot
    | Target
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module MentionName =
    let toString (m: MentionName) =
        match m with
        | Author -> "author"
        | Bot -> "bot"
        | Target -> "target"

    let parse: MentionName Parser =
        choice [
            skipStringCI (toString Author) >>% Author
            skipStringCI (toString Bot) >>% Bot
            skipStringCI (toString Target) >>% Target
        ]

type CustomMention = MentionName * MentionType
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module CustomMention =
    let toString  ((name, typ): CustomMention) =
        sprintf "%s%s" (MentionName.toString name) (MentionType.toString typ)

    let parse: _ Parser =
        MentionName.parse .>>. MentionType.parse

type Substitions = Map<string, Lazy<string> option>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Substitions =
    let create
        (author: DSharpPlus.Entities.DiscordUser)
        (bot: DSharpPlus.Entities.DiscordUser)
        (target: DSharpPlus.Entities.DiscordUser option): Substitions =

        let createValueName (user: DSharpPlus.Entities.DiscordUser) =
            lazy (user.Username)

        let createValueId (user: DSharpPlus.Entities.DiscordUser) =
            lazy (sprintf "<@%d>" user.Id)

        let createName (mentionName, user) =
            CustomMention.toString (mentionName, MentionType.Name), user |> Option.map createValueName

        let createMention (mentionName, user) =
            CustomMention.toString (mentionName, MentionType.Mention), user |> Option.map createValueId

        let xs =
            [|
                MentionName.Author, Some author
                MentionName.Bot, Some bot
                MentionName.Target, target
            |]

        Map [|
            yield! xs |> Array.map createName
            yield! xs |> Array.map createMention
        |]

    let get (customMention: CustomMention) (substitions: Substitions) =
        let rawCustomMention = CustomMention.toString customMention
        match Map.tryFind rawCustomMention substitions with
        | Some x -> x
        | None ->
            failwithf "not found %s" rawCustomMention

/// Part of message template
type MessagePart =
    | Text of string
    | CustomMention of CustomMention

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module MessagePart =
    let toString = function
        | Text x -> x
        | CustomMention(name, typ) ->
            sprintf "<@%s>" (CustomMention.toString (name, typ))

    let parse: MessagePart Parser =
        let praw = many1Satisfy ((<>) '<')

        let pcustomMention =
            puserMentionTargetP CustomMention.parse

        choice [
            praw |>> Text
            pcustomMention |>> CustomMention
            pstring "<" |>> Text
        ]

    let substitute (substitions: Substitions) = function
        | Text x -> x
        | CustomMention x ->
            match Substitions.get x substitions with
            | Some x -> x.Value
            | None -> CustomMention.toString x

type MessageRaw = string

type Message = MessagePart list

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Message =
    let parser: Message Parser =
        many MessagePart.parse

    let parse message =
        FParsecExt.runResult parser message

    let toString (template: Message): MessageRaw =
        template |> List.map MessagePart.toString |> System.String.Concat

    let substitute (substitions: Substitions) (message: Message) =
        message
        |> List.map (MessagePart.substitute substitions)
        |> System.String.Concat
