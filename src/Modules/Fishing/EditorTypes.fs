module Fishing.Editor.Types
open FsharpMyExtension

type Version =
    | V0 = 0
    | V1 = 1

type ItemId = System.Guid

type Loot = ItemId []

type ItemV0 =
    {
        Version: int
        ItemId: ItemId
        Name: string
        Loot: ItemId []
        Description: string
        ImageUrl: string
    }

type Item =
    {
        Version: Version
        Id: ItemId
        Name: string
        AsBait: Option<Loot>
        AsChest: Option<Loot>
        Description: string
        ImageUrl: Option<string>
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
    let serializerWithOption =
        let optConv = FSharpJsonType.SerializeOption.converter
        let ser = Newtonsoft.Json.JsonSerializer()
        ser.Converters.Add optConv
        ser

    let tryDeserialize json =
        try
            let res: Items<MapItemRaw> = Json.des json

            res.value
            |> Array.map (function
                | [|id; item|] ->
                    let item = item :?> Newtonsoft.Json.Linq.JObject

                    let version =
                        let x = item.Item "Version"
                        x.ToObject<Version>()

                    match version with
                    | Version.V0 ->
                        let oldItem = item.ToObject<ItemV0>()
                        {
                            Version = Version.V1
                            Id = oldItem.ItemId
                            Name = oldItem.Name
                            AsBait = Some oldItem.Loot
                            AsChest = None
                            Description = oldItem.Description
                            ImageUrl = Some oldItem.ImageUrl
                        }
                    | Version.V1 ->
                        item.ToObject<Item>(serializerWithOption)
                    | unknownVersion ->
                        failwithf "unknownVersion — %A" unknownVersion
                | xs ->
                    failwithf "expected [|id; item|] but %A" xs
            )
            |> Ok
        with e ->
            Error e.Message
