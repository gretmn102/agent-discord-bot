module Modules.CustomCommand.Model.Tests
open Fuchu

open CustomCommand.Model

let equal act exp msg =
    Assert.Equal(msg, exp, act)

[<Tests>]
let ``ReactionsList.randomGetWithCustomRandom`` =
    let createReaction prob msg =
        Reaction.create
            prob
            (Message.create (Some msg) Embed.empty)

    testList "ReactionsList.randomGetWithCustomRandom" [
        testCase "only one of {1}" <| fun () ->
            let first = createReaction 1 "first"

            equal
                ([| first |] |> ReactionsList.randomGetWithCustomRandom (fun min max -> 0))
                first
                ""

        testCase "first of {1, 1}" <| fun () ->
            let first = createReaction 1 "first"
            let second = createReaction 1 "second"

            equal
                ([| first; second |]
                |> ReactionsList.randomGetWithCustomRandom (fun min max ->
                    equal min 0 ""
                    equal max 2 ""
                    0
                ))
                first
                ""

        testCase "second of {1, 1}" <| fun () ->
            let first = createReaction 1 "first"
            let second = createReaction 1 "second"

            equal
                ([| first; second |]
                |> ReactionsList.randomGetWithCustomRandom (fun min max ->
                    equal min 0 ""
                    equal max 2 ""
                    1
                ))
                second
                ""

        testCase "second of {1, 20}" <| fun () ->
            let first = createReaction 1 "first"
            let second = createReaction 20 "second"

            equal
                ([| first; second |]
                |> ReactionsList.randomGetWithCustomRandom (fun min max ->
                    equal min 0 ""
                    equal max 21 ""
                    2
                ))
                second
                ""

        testCase "last of {1, 10, 1}" <| fun () ->
            let first = createReaction 1 "first"
            let second = createReaction 10 "second"
            let last = createReaction 1 "last"

            equal
                ([| first; second; last |]
                |> ReactionsList.randomGetWithCustomRandom (fun min max ->
                    equal min 0 ""
                    equal max 12 ""
                    11
                ))
                last
                ""
    ]
