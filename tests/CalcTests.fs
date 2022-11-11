module CalcTests
open Fuchu
open FsharpMyExtension
open FsharpMyExtension.Either

open Calc.Core

[<Tests>]
let pexprTests =
    testList "pexprTests" [
        testCase "1 + 2 * 3" <| fun () ->
            let input = "1 + 2 * 3"
            let exp = Right (Binary (Plus, Val (Int32 1), Binary (Times, Val (Int32 2), Val (Int32 3))))
            let act = FParsecExt.runEither Parser.pexpr input

            Assert.Equal("", exp, act)

        testCase "(1 + 2) * 3" <| fun () ->
            let input = "(1 + 2) * 3"
            let exp = Right (Binary (Times, Binary (Plus, Val (Int32 1), Val (Int32 2)), Val (Int32 3)))
            let act = FParsecExt.runEither Parser.pexpr input

            Assert.Equal("", exp, act)

        testCase "1.0 + 2.0 * 3" <| fun () ->
            let input = "1.0 + 2.0 * 3"
            let exp = Right (Binary (Plus, Val (Float 1.0), Binary (Times, Val (Float 2.0), Val (Int32 3))))
            let act = FParsecExt.runEither Parser.pexpr input

            Assert.Equal("", exp, act)

        testCase "1.0 + 2.0 * -(3 + 4)" <| fun () ->
            let input = "1.0 + 2.0 * -(3 + 4)"
            let exp =
                Right
                    (Binary
                        (Plus, Val (Float 1.0),
                         Binary
                            (Times, Val (Float 2.0),
                             Unary (Neg, Binary (Plus, Val (Int32 3), Val (Int32 4))))))
            let act = FParsecExt.runEither Parser.pexpr input

            Assert.Equal("", exp, act)

        testCase "1 * 2 ** 3" <| fun () ->
            let input = "1 * 2 ** 3"
            let exp = Right (Binary (Times, Val (Int32 1), Binary (Pow, Val (Int32 2), Val (Int32 3))))
            let act = FParsecExt.runEither Parser.pexpr input

            Assert.Equal("", exp, act)
    ]
