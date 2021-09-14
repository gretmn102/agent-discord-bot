module FParsecUtilsTests
open FsharpMyExtension
open FsharpMyExtension.Either
open FParsec
open FParsecUtils
open Fuchu


[<Tests>]
let pbigintTests =
    testList "pbigintTests" [
        testCase "testCase1" (fun _ ->
            let exp = 1234I
            let act = runEither pbigint (string exp)

            Assert.Equal("", act, Right exp)
        )
        testCase "testCase2" (fun _ ->
            let exp = 12345678901234567890I
            let act = runEither pbigint (string exp)

            Assert.Equal("", act, Right exp)
        )
    ]
