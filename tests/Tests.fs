open Fuchu
open FsharpMyExtension
open FsharpMyExtension.Either

#if INTERACTIVE
#r @"C:\Users\User\.nuget\packages\dsharpplus\4.1.0\lib\netstandard2.0\DSharpPlus.dll"
#load @"..\src\MainProj\Types.fs"
#load @"..\src\MainProj\CommandParser.fs"
#endif
open CommandParser

// [<Tests>]
// let commandParserTests =
//     testList "commandParserTests" [
//         testCase "testCase1" (fun _ ->
//             let botId = 0UL
//             Assert.Equal("msg1", Right Pass, start botId "")
//             Assert.Equal("msg2", Right Unknown, start botId (sprintf "<@%d>" botId))
//             Assert.Equal("msg3", Right (CustomCommandCmd (CustomCommand.Main.Take, None)), start botId ".take")

//             let whom = 1UL
//             Assert.Equal("msg4", Right (CustomCommandCmd (CustomCommand.Main.Take, Some whom)), start botId (sprintf ".take <@%d>" whom))
//             Assert.Equal("msg5", Right Pass, start botId ".unknown")

//             let exp =
//                 [
//                     "Error in Ln: 1 Col: 7"
//                     sprintf "<@%d> .unknown" botId
//                     "      ^"
//                     "Expecting:"
//                 ] |> String.concat "\r\n"

//             match start botId (sprintf "<@%d> .unknown" botId) with
//             | Left errMsg ->
//                 Assert.StringContains("msg6", exp, errMsg)
//             | Right _ as act ->
//                 failwithf "Expected:\n%A\nActual:\n%A" (Left exp) act

//             Assert.Equal("not mention bot", Right Pass, start botId "<@1234567> .unknown")
//             Assert.Equal("not mention bang bot", Right Pass, start botId "<@!1234567> .unknown")
//         )
//     ]

[<Tests>]
let pshipTests =
    let pship = Ship.Main.Parser.pship

    testList "pshipTests" [
        testCase "pshipTestsShipRand" (fun _ ->
            Assert.Equal("", Right Ship.Main.Rand, FParsecExt.runEither pship "shipRand")
        )
        testCase "pshipTestsShip0" (fun _ ->
            Assert.Equal("", Right (Ship.Main.Target 0), FParsecExt.runEither pship "ship0")
        )
        testCase "pshipTestsShip100" (fun _ ->
            Assert.Equal("", Right (Ship.Main.Target 100), FParsecExt.runEither pship "ship100")
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
            Assert.Equal("", Left exp, FParsecExt.runEither pship "ship")
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
                Assert.Equal("", exp, FParsecExt.runEither ptemplateMessage input)
            )
        ]

[<EntryPoint;System.STAThread>]
let main arg =
    System.Environment.CurrentDirectory <-
        System.IO.Path.Combine(System.Environment.CurrentDirectory, @"bin/Release/net6.0")

    defaultMainThisAssembly arg
