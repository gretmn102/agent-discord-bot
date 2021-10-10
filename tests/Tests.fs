open Fuchu
open FsharpMyExtension
open FsharpMyExtension.Either

#if INTERACTIVE
#r @"C:\Users\User\.nuget\packages\dsharpplus\4.1.0\lib\netstandard2.0\DSharpPlus.dll"
#load @"..\src\MainProj\Types.fs"
#load @"..\src\MainProj\CommandParser.fs"
#endif
open CommandParser

[<Tests>]
let commandParserTests =
    testList "commandParserTests" [
        testCase "testCase1" (fun _ ->
            let botId = 0UL
            Assert.Equal("msg1", Right Pass, start botId "")
            Assert.Equal("msg2", Right Unknown, start botId (sprintf "<@%d>" botId))
            Assert.Equal("msg3", Right (Act (Take, None)), start botId ".take")

            let whom = 1UL
            Assert.Equal("msg4", Right (Act (Take, Some whom)), start botId (sprintf ".take <@%d>" whom))
            Assert.Equal("msg5", Right Pass, start botId ".unknown")

            let act =
                [
                    "Error in Ln: 1 Col: 7"
                    sprintf "<@%d> .unknown" botId
                    "      ^"
                    "Expecting: 'admire', 'ballotBox', 'battery', 'bully', 'catail', 'cyoa',"
                    "'fairytail', 'quiz', 'quizPizza', 'quizWithMultiChoices', 'ship',"
                    "'someGirlsQuiz', 'take', 'catch' (case-insensitive), 'emojiFont'"
                    "(case-insensitive), 'massShip' (case-insensitive) or 'numberToWords'"
                    "(case-insensitive)"
                    ""
                ] |> String.concat "\r\n"
            Assert.Equal("msg6", Left act, start botId (sprintf "<@%d> .unknown" botId))
            Assert.Equal("not mention bot", Right Pass, start botId "<@1234567> .unknown")
            Assert.Equal("not mention bang bot", Right Pass, start botId "<@!1234567> .unknown")
        )
    ]

[<Tests>]
let pshipTests =
    testList "pshipTests" [
        testCase "pshipTestsShipRand" (fun _ ->
            Assert.Equal("", Right Rand, FParsecUtils.runEither pship "shipRand")
        )
        testCase "pshipTestsShip0" (fun _ ->
            Assert.Equal("", Right (Target 0), FParsecUtils.runEither pship "ship0")
        )
        testCase "pshipTestsShip100" (fun _ ->
            Assert.Equal("", Right (Target 100), FParsecUtils.runEither pship "ship100")
        )
        testCase "pshipTestsErr" (fun _ ->
            let exp =
                [
                    "Error in Ln: 1 Col: 1"
                    "ship"
                    "^"
                    ""
                    "The parser backtracked after:"
                    "  Error in Ln: 1 Col: 5"
                    "  ship"
                    "      ^"
                    "  Note: The error occurred at the end of the input stream."
                    "  Expecting: integer number (32-bit, signed) or 'rand' (case-insensitive)"
                    ""
                ] |> String.concat "\r\n"
            Assert.Equal("", Left exp, FParsecUtils.runEither pship "ship")
        )
    ]

[<Tests>]
let ballotBoxTests =
    testList "ballotBoxTests" [
        testCase "ballotBoxTests1" (fun _ ->
            let input =
                [
                    "ballotBox Нужны ли нам такие голосовалки?"
                    "Да"
                    "Нет"
                    "Удоли!11"
                    "Vox Populi, Vox Dei"
                ] |> String.concat "\n"
            let exp =
                Right
                  (BallotBox
                     ("Нужны ли нам такие голосовалки?",
                      ["Да"; "Нет"; "Удоли!11"; "Vox Populi, Vox Dei"]))
            Assert.Equal("", exp, FParsecUtils.runEither pballotBox input)
        )
        testCase "ballotBoxTests2" (fun _ ->
            let input =
                [
                    "ballotBox Нужны ли нам такие голосовалки?"
                    "Да"
                    "Нет"
                    ""
                    "Vox Populi, Vox Dei"
                ] |> String.concat "\n"
            let exp =
                Right
                  (BallotBox
                     ("Нужны ли нам такие голосовалки?", ["Да"; "Нет"; "Vox Populi, Vox Dei"]))
            Assert.Equal("", exp, FParsecUtils.runEither pballotBox input)
        )
    ]

[<EntryPoint;System.STAThread>]
let main arg =
    defaultMainThisAssembly arg
