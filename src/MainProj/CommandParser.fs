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
    | BallotBox of description:string * choices:string list
    | NumberToWords of bigint

    | MessageCreateEventHandler of MessageCreateEventHandler

let prefix = pchar '.'

let pballotBox =
    pstring "ballotBox"
    >>. many1Satisfy ((=) ' ') >>. many1Satisfy ((<>) '\n') .>> spaces
    .>>. many1 (many1Satisfy ((<>) '\n') .>> spaces)

let pcommand: _ Parser =
    let pmessageCreateEventHandler = choice [
        CustomCommand.Main.exec
        UserRole.Main.exec
        Doorkeeper.Main.exec
        VoiceChannelNotification.Main.exec
        Ranking.Main.exec
        Music.Main.exec
        MessageManager.exec
        ReactionEvent.Main.exec
        Birthday.Main.exec
        Events.Main.exec
        ChatVoice.Main.exec
        DiscordWebhook.Main.exec
        Boosters.Main.exec
        Doorkeeper.Invites.exec
        UserInfo.Main.exec
        Age.Main.exec
        EggBattle.Main.exec
        Moderation.Main.exec
        Ship.Main.exec
        EmojiFont.Main.exec
        Calc.Main.exec
        Roll.Main.exec
        SimpleQuiz.Main.exec
        // Fishing.Main.exec
    ]

    choice [
        stringReturn "someGirlsQuiz" (Cyoa AppsHub.Hub.SomeGirlsQuiz)
        stringReturn "cyoa" (Cyoa AppsHub.Hub.SomeCyoa)
        stringReturn "quizWithMultiChoices" (Cyoa AppsHub.Hub.QuizWithMultiChoices)
        stringReturn "quizPizza" (Cyoa AppsHub.Hub.QuizPizza)
        stringReturn "quiz" SomeQuiz

        pmessageCreateEventHandler |>> MessageCreateEventHandler

        pstringCI "numberToWords" >>. spaces >>. FParsecUtils.pbigint |>> NumberToWords

        pballotBox |>> BallotBox
    ]

let start botId str =
    let p =
        (attempt (puserMentionTarget botId) >>. spaces >>. (optional prefix >>. pcommand <|>% Unknown))
        <|> ((prefix >>? pcommand) <|>% Pass)
    match run p str with
    | Success(x, _, _) -> Right x
    | Failure(x, _, _) -> Left x
