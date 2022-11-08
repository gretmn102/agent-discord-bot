module Fishing.Editor.Types
open FsharpMyExtension

type Item =
    {
        Version: int
        ItemId: System.Guid
        Name: string
        Loot: System.Guid []
        Description: string
    }

type MapItemRaw = obj []

type Items<'MapItem> =
    {
        dataType: string
        value: 'MapItem []
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Items =
    let tryDeserialize json =
        try
            let res: Items<MapItemRaw> = Json.des json

            res.value
            |> Array.map (function
                | [|id; item|] ->
                    let item = item :?> Newtonsoft.Json.Linq.JObject
                    item.ToObject<Item>()
                | xs ->
                    failwithf "expected [|id; item|] but %A" xs
            )
            |> Ok
        with e ->
            Error e.Message
