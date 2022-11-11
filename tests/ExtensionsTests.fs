module ExtensionsTests
open Fuchu

module Interaction =
    open Extensions.Interaction

    type ComponentId =
        | List = 0

    type ComponentState = ComponentState<ComponentId, string>

    [<Tests>]
    let componentStateSerializationTests =
        testList "componentStateSerializationTests" [
            testCase "base" <| fun () ->
                let exp: ComponentState =
                    ComponentState.create "comp\\\n1\nz" ComponentId.List "12\\1\\n3\\\\4"

                let act =
                    exp
                    |> ComponentState.serialize ComponentState.Printer.showEsapedString
                    |> ComponentState.tryDeserialize ComponentState.Parser.pescapedString

                Assert.Equal("", Some (Ok exp), act)
        ]
