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

type CustomEmoji =
    {
        Id: EmojiId
        Animated: bool
        Name: string
    }

let pcustomEmoji: _ Parser =
    pipe3
        (stringReturn "<:" false <|> stringReturn "<a:" true)
        (manySatisfy ((<>) ':') .>> skipChar ':')
        (puint64 .>> pchar '>')
        (fun animated name id ->
            {
                Id = id
                Animated = animated
                Name = name
            }
        )

type ShipOption =
    | Rand
    | Target of int

type Act =
    | Take
    | Fairytail
    | Catail
    | Bully
    | Admire
    | Battery
    | Ship of ShipOption
    | Catch

type UnicodeOrCustomEmoji =
    | UnicodeEmoji of string
    | CustomEmoji of CustomEmoji

type Cmd =
    | Act of Act * UserId option
    | MassShip of UserId list
    | Cyoa of AppsHub.Hub.CyoaT
    | SomeQuiz
    | Unknown
    | Pass
    | BallotBox of description:string * choices:string list
    | NumberToWords of bigint
    | EmojiFont of UnicodeOrCustomEmoji * string

let prefix = pchar '.'

let pballotBox =
    pstring "ballotBox"
    >>. many1Satisfy ((=) ' ') >>. many1Satisfy ((<>) '\n') .>> spaces
    .>>. many1 (many1Satisfy ((<>) '\n') .>> spaces)
    |>> BallotBox

let pship : _ Parser =
    let ptarget =
        pint32
        >>= fun x ->
           if 0 <= x && x <= 100 then
               preturn x
           else
               fail "Значение должно быть от 0 до 100 включительно"

    skipString "ship"
    >>? ((ptarget |>> Target) <|> (skipStringCI "rand" >>% Rand))

let pmassShip =
    pipe2
        (skipStringCI "massShip" .>> spaces)
        (many (puserMention .>> spaces))
        (fun cmd usersIds -> MassShip usersIds)

let pcommand =
    let cmd =
        choice [
            skipString "take" >>% Take
            skipString "fairytail" >>% Fairytail
            skipString "catail" >>% Catail
            skipString "bully" >>. optional (skipString "ing") >>% Bully
            skipString "admire" >>% Admire
            skipString "battery" >>% Battery
            skipStringCI "catch" >>% Catch
            pship |>> Ship
        ]
    choice [
        cmd .>> spaces .>>. opt puserMention |>> Act
        stringReturn "someGirlsQuiz" (Cyoa AppsHub.Hub.SomeGirlsQuiz)
        stringReturn "cyoa" (Cyoa AppsHub.Hub.SomeCyoa)
        stringReturn "quizWithMultiChoices" (Cyoa AppsHub.Hub.QuizWithMultiChoices)
        stringReturn "quizPizza" (Cyoa AppsHub.Hub.QuizPizza)
        stringReturn "quiz" SomeQuiz
        pstringCI "numberToWords" >>. spaces >>. FParsecUtils.pbigint |>> NumberToWords
        pballotBox
        pmassShip

        skipStringCI "emojiFont" >>. spaces
        >>. (pcustomEmoji |>> CustomEmoji <|> (many1Satisfy ((<>) ' ') |>> UnicodeEmoji) .>> spaces)
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
