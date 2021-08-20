open Fuchu
open FsharpMyExtension
open FsharpMyExtension.Either

#if INTERACTIVE
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
            Assert.Equal("msg3", Right (Take None), start botId ".take")

            let whom = 1UL
            Assert.Equal("msg4", Right (Take (Some whom)), start botId (sprintf ".take <@%d>" whom))
            Assert.Equal("msg5", Right Pass, start botId ".unknown")

            let act =
                [
                    "Error in Ln: 1 Col: 7"
                    sprintf "<@%d> .unknown" botId
                    "      ^"
                    "Expecting: 'take'"
                    ""
                ] |> String.concat "\r\n"
            Assert.Equal("msg6", Left act, start botId (sprintf "<@%d> .unknown" botId))
        )
    ]

[<EntryPoint;System.STAThread>]
let main arg =
    defaultMainThisAssembly arg
