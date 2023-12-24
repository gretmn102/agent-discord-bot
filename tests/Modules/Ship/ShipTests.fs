module Modules.Ship.Tests
open FsharpMyExtension
open FsharpMyExtension.Either
open Fuchu

open Ship.Main

[<Tests>]
let ``Ship.Main.Parser.parser`` =
    let pship = Parser.pship

    testList "Ship.Main.Parser.parser" [
        testCase "shipRand" <| fun _ ->
            Assert.Equal("", Right Rand, FParsecExt.runEither pship "shipRand")

        testCase "ship0" <| fun _ ->
            Assert.Equal("", Right (Target 0), FParsecExt.runEither pship "ship0")

        testCase "ship100" <| fun _ ->
            Assert.Equal("", Right (Target 100), FParsecExt.runEither pship "ship100")

        testCase "empty ship" <| fun _ ->
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
                ] |> String.concat System.Environment.NewLine
            Assert.Equal("", Left exp, FParsecExt.runEither pship "ship")
    ]

[<Tests>]
let getDescriptionTests =
    testList "getDescriptionTests" [
        testCase "0" <| fun () ->
            let act = getDescription 0
            let exp = "Ничто в мире не даётся без труда — даже любовь..."
            Assert.Equal("", exp, act)

        testCase "99" <| fun () ->
            let act = getDescription 99
            let exp = "За такие чувства не жалко и умереть!"
            Assert.Equal("", exp, act)

        testCase "100" <| fun () ->
            let act = getDescription 100
            let exp = "Дошипперились? Теперь женитесь и живите долго и счастливо"
            Assert.Equal("", exp, act)
    ]
