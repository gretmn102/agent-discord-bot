module Modules.BallotBox.Tests
open FsharpMyExtension
open FsharpMyExtension.Either
open Fuchu

open BallotBox.Main

[<Tests>]
let pcreateBallotBoxTests =
    testList "pcreateBallotBoxTests" [
        testCase "pcreateBallotBoxTests1" (fun _ ->
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
                  ("Нужны ли нам такие голосовалки?", ["Да"; "Нет"; "Удоли!11"; "Vox Populi, Vox Dei"])
            Assert.Equal("", exp, FParsecExt.runEither Parser.pcreateBallotBox input)
        )
        testCase "pcreateBallotBoxTests2" (fun _ ->
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
                  ("Нужны ли нам такие голосовалки?", ["Да"; "Нет"; "Vox Populi, Vox Dei"])
            Assert.Equal("", exp, FParsecExt.runEither Parser.pcreateBallotBox input)
        )
    ]
