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
                    "'someGirlsQuiz', 'take', 'addPermissiveRole' (case-insensitive), 'angry'"
                    "(case-insensitive), 'catch' (case-insensitive), 'emojiFont' (case-insensitive),"
                    "'massShip' (case-insensitive), 'newcomersRoles' (case-insensitive),"
                    "'numberToWords' (case-insensitive), 'permissiveRoles' (case-insensitive),"
                    "'removePermissiveRole' (case-insensitive), 'removeUserRole' (case-insensitive),"
                    "'role' (case-insensitive), 'setNewcomersRoles' (case-insensitive),"
                    "'setTemplateRole' (case-insensitive), 'setVoiceNotificationOutput'"
                    "(case-insensitive), 'setVoiceNotificationTemplateMsg' (case-insensitive),"
                    "'updateUserRolesPermissions' (case-insensitive) or 'userRoles'"
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

module VoiceChannelNotificationTests =
    open VoiceChannelNotification.Main.Parser

    [<Tests>]
    let templateMessageTests =
        testList "templateMessageTests" [
            testCase "templateMessageTests1" (fun _ ->
                let input = "<:lpPepeLol:923837721481469992> <@3847> <@!1234> <@nickName> заш<@userName>ел в <#voiceChannel>!"
                let exp = Right [
                   Text "<"; Text ":lpPepeLol:923837721481469992> "
                   Text "<"; Text "@3847> "
                   Text "<"; Text "@!1234> "
                   NickName
                   Text " заш"; UserName; Text "ел в "
                   VoiceChannel; Text "!"
                ]
                Assert.Equal("", exp, FParsecUtils.runEither ptemplateMessage input)
            )
        ]

[<EntryPoint;System.STAThread>]
let main arg =
    defaultMainThisAssembly arg
