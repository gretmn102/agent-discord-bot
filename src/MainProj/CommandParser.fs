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
    | EmojiFontCmd of EmojiFont.Main.Request

    | UserRoleCmd of UserRole.Main.Request

    | Doorkeeper of Doorkeeper.Main.Request

    | VoiceChannelNotification of VoiceChannelNotification.Main.VoiceNotificationMsg

    | RankingCmd of Ranking.Main.Request

    | MusicCmd of Music.Main.Request

    | MessageManagerCmd of MessageManager.Request

    | ReactionEventCmd of ReactionEvent.Main.Request

    | BirthdayCmd of Birthday.Main.Request

    | EventsCmd of Events.Main.SettingMsg

    | ShipCmd of Ship.Main.Msg

    | ChatVoiceCmd of ChatVoice.Main.Request

    | DiscordWebhookCmd of DiscordWebhook.Main.Request

    | BoostersCmd of Boosters.Main.Request

    | InvitesCmd of Doorkeeper.Invites.Request

    | UserInfoCmd of UserInfo.Main.Request

    | AgeCmd of Age.Main.Request

    | EggBattleCmd of EggBattle.Main.Request

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

        UserRole.Main.Parser.start |>> UserRoleCmd

        Doorkeeper.Main.Parser.start |>> Doorkeeper

        VoiceChannelNotification.Main.Parser.start |>> VoiceChannelNotification

        Ranking.Main.Parser.start |>> RankingCmd

        Music.Main.Parser.start |>> MusicCmd

        MessageManager.Parser.start |>> MessageManagerCmd

        ReactionEvent.Main.Parser.start |>> ReactionEventCmd

        Birthday.Main.Parser.start |>> BirthdayCmd

        Events.Main.Parser.start |>> EventsCmd

        ChatVoice.Main.Parser.start |>> ChatVoiceCmd

        DiscordWebhook.Main.Parser.start |>> DiscordWebhookCmd

        Boosters.Main.Parser.start |>> BoostersCmd

        Doorkeeper.Invites.Parser.start |>> InvitesCmd

        UserInfo.Main.Parser.start |>> UserInfoCmd

        Age.Main.Parser.start |>> AgeCmd

        EggBattle.Main.Parser.start |>> EggBattleCmd

        pstringCI "numberToWords" >>. spaces >>. FParsecUtils.pbigint |>> NumberToWords
        pballotBox

        Ship.Main.Parser.start |>> ShipCmd

        EmojiFont.Main.Parser.start |>> EmojiFontCmd
    ]

let start botId str =
    let p =
        (attempt (puserMentionTarget botId) >>. spaces >>. (optional prefix >>. pcommand <|>% Unknown))
        <|> ((prefix >>? pcommand) <|>% Pass)
    match run p str with
    | Success(x, _, _) -> Right x
    | Failure(x, _, _) -> Left x
