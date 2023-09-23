module Modules.Ship.Tests
open FsharpMyExtension
open FsharpMyExtension.Either
open Fuchu

open Ship.Main

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
