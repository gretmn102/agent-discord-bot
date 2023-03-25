open Fuchu
open FsharpMyExtension
open FsharpMyExtension.Either

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
        System.IO.Path.Combine(System.Environment.CurrentDirectory, @"bin/Release/netcoreapp3.1")

    defaultMainThisAssembly arg
