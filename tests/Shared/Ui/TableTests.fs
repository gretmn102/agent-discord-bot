module TableTests
open Fuchu

open Shared.Ui.Table

type SortBy =
    | SortByName = 0
    | SortByFunction = 1

[<Tests>]
let sortByContainerTests =
    let sample =
        [|
            SortBy.SortByName, "Sort by name"
            SortBy.SortByFunction, "Sort by function"
        |]
        |> SortByContainer.Init

    testList "SortByContainerTests" [
        testCase "GetName" <| fun () ->
            Assert.Equal("", "Sort by name", sample.GetDescription SortBy.SortByName)

        testCase "GetValue1" <| fun () ->
            let exp = SortBy.SortByName
            let act = sample.ToString exp |> sample.GetValue
            Assert.Equal("", exp, act)

        testCase "GetValue2" <| fun () ->
            let exp = SortBy.SortByFunction
            let act = sample.ToString exp |> sample.GetValue
            Assert.Equal("", exp, act)
    ]
