module CommandParser
open FsharpMyExtension
open FsharpMyExtension.Either
open FParsec

open Types
open DiscordMessage
open DiscordMessage.Parser

type 'a Parser = Parser<'a, unit>

type Cmd =
    | CustomCommandCmd of CustomCommand.Main.Msg
    | Cyoa of AppsHub.Hub.CyoaT
    | SomeQuiz
    | Unknown
    | Pass
    | BallotBox of description:string * choices:string list
    | NumberToWords of bigint
    | EmojiFont of UnicodeOrCustomEmoji * string

    | Role of Role.Main.RoleEditModel
    | AddPermissiveRole of RoleId
    | RemovePermissiveRole of RoleId
    | GetPermissiveRoles
    | GetUserRoles
    | RemoveUserRole of RoleId
    | SetTemplateRole of RoleId
    | UpdateUserRolesPermissions

    | Doorkeeper of Doorkeeper.Main.Request

    | VoiceChannelNotification of VoiceChannelNotification.Main.VoiceNotificationMsg

    | RankingCmd of Ranking.Main.Request

    | MusicCmd of Music.Main.Request

    | MessageManagerCmd of MessageManager.Request

    | ReactionEventCmd of ReactionEvent.Main.Request

    | BirthdayCmd of Birthday.Main.Request

    | EventsCmd of Events.Main.SettingMsg

    | ShipCmd of Ship.Main.Msg

let prefix = pchar '.'

let pballotBox =
    pstring "ballotBox"
    >>. many1Satisfy ((=) ' ') >>. many1Satisfy ((<>) '\n') .>> spaces
    .>>. many1 (many1Satisfy ((<>) '\n') .>> spaces)
    |>> BallotBox

let pcommand: _ Parser =
    choice [
        CustomCommand.Main.Parser.start |>> CustomCommandCmd

        stringReturn "someGirlsQuiz" (Cyoa AppsHub.Hub.SomeGirlsQuiz)
        stringReturn "cyoa" (Cyoa AppsHub.Hub.SomeCyoa)
        stringReturn "quizWithMultiChoices" (Cyoa AppsHub.Hub.QuizWithMultiChoices)
        stringReturn "quizPizza" (Cyoa AppsHub.Hub.QuizPizza)
        stringReturn "quiz" SomeQuiz

        Role.Main.Parser.pgiveOrChangeRole |>> Role
        Role.Main.Parser.paddPermissiveRole |>> AddPermissiveRole
        Role.Main.Parser.premovePermissiveRole |>> RemovePermissiveRole
        Role.Main.Parser.pgetPermissiveRoles >>% GetPermissiveRoles
        Role.Main.Parser.pgetUserRoles >>% GetUserRoles
        Role.Main.Parser.premoveUserRole |>> RemoveUserRole
        Role.Main.Parser.psetTemplateRole |>> SetTemplateRole
        Role.Main.Parser.pupdateUserRolesPermissions >>% UpdateUserRolesPermissions

        Doorkeeper.Main.Parser.start |>> Doorkeeper

        VoiceChannelNotification.Main.Parser.start |>> VoiceChannelNotification

        Ranking.Main.Parser.start |>> RankingCmd

        Music.Main.Parser.start |>> MusicCmd

        MessageManager.Parser.start |>> MessageManagerCmd

        ReactionEvent.Main.Parser.start |>> ReactionEventCmd

        Birthday.Main.Parser.start |>> BirthdayCmd

        Events.Main.Parser.start |>> EventsCmd

        pstringCI "numberToWords" >>. spaces >>. FParsecUtils.pbigint |>> NumberToWords
        pballotBox

        Ship.Main.Parser.start |>> ShipCmd

        skipStringCI "emojiFont" >>. spaces
        >>. (pemoji .>> spaces)
        .>>. manySatisfy (fun _ -> true)
        |>> EmojiFont
    ]

let start botId str =
    let p =
        (attempt (puserMentionTarget botId) >>. spaces >>. (optional prefix >>. pcommand <|>% Unknown))
        <|> ((prefix >>? pcommand) <|>% Pass)
    match run p str with
    | Success(x, _, _) -> Right x
    | Failure(x, _, _) -> Left x
