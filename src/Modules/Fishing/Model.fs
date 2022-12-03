module Fishing.Model
open FsharpMyExtension
open MongoDB.Driver
open MongoDB.Bson

open Types
open Db

type ItemId = System.Guid
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module ItemId =
    let serialize (id: ItemId) =
        id.ToString()

    let tryDeserialize (str: string) =
        match System.Guid.TryParse str with
        | true, x -> Ok (x: ItemId)
        | false, _ -> Error ""

module ItemsDb =
    type ItemData =
        {
            Name: string
            AsBait: Option<Editor.Types.Loot>
            AsChest: Option<Editor.Types.Loot>
            Description: string
            ImageUrl: Option<string>
        }
        static member Create name asBait asChest description imageUrl: ItemData =
            {
                Name = name
                AsBait = asBait
                AsChest = asChest
                Description = description
                ImageUrl = imageUrl
            }
        static member Empty: ItemData =
            {
                Name = ""
                AsBait = None
                AsChest = None
                Description = ""
                ImageUrl = Some ""
            }
        static member Serialize (data: ItemData) =
            data |> Json.ser
        static member Deserialize json =
            try
                Ok (Json.des json)
            with e ->
                Error e.Message

    type Version =
        | V0 = 0

    type ItemT = CommonDb.Data<ItemId, Version, ItemData>

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Item =
        let create id data: ItemT =
            CommonDb.Data.create id Version.V0 data

        let isBait (item: ItemT) =
            match item.Data.AsBait with
            | Some xs -> not (Array.isEmpty xs)
            | None -> false

        let isChest (item: ItemT) =
            match item.Data.AsChest with
            | Some xs -> not (Array.isEmpty xs)
            | None -> false

        let ofEditorItem (item: Editor.Types.Item): ItemT =
            let x =
                {
                    Name = item.Name
                    AsBait = item.AsBait
                    AsChest = item.AsChest
                    Description = item.Description
                    ImageUrl = item.ImageUrl
                }
            create item.Id x

    type ItemsArray = ItemT []
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module ItemsArray =
        let serialize (item: ItemsArray) =
            Json.ser item

        let tryDeserialize json =
            try
                let res: ItemsArray = Json.des json
                Ok res
            with e ->
                Error e.Message

    type Items = CommonDb.GuildData<ItemId, Version, ItemData>

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Items =
        let createData id =
            Item.create id ItemData.Empty

        let init collectionName (db: IMongoDatabase): Items =
            CommonDb.GuildData.init
                createData
                (fun ver doc ->
                    match Option.get ver with
                    | Version.V0 ->
                        None, Serialization.BsonSerializer.Deserialize<ItemT>(doc)
                    | x ->
                        failwithf "Version = %A not implemented" x
                )
                collectionName
                db

        let set itemIdOpt setAdditionParams (items: Items) =
            CommonDb.GuildData.set
                createData
                (itemIdOpt |> Option.defaultWith (fun () -> System.Guid.NewGuid()))
                setAdditionParams
                items

        let setItems (items: ItemsArray) (itemsDb: Items): Items =
            items
            |> Array.fold
                (fun items item ->
                    let rec f () =
                        try
                            set (Some item.Id) (fun _ -> item.Data) items
                        with e ->
                            printfn "setItems error:\n%s" e.Message
                            System.Threading.Thread.Sleep 500
                            f ()
                    f ()
                )
                itemsDb

        let drop (db: IMongoDatabase) (items: Items) =
            CommonDb.GuildData.drop db items

        let tryFindById id (items: Items): ItemT option =
            CommonDb.GuildData.tryFind id items

        let tryFindByName name (items: Items): ItemT option =
            items.Cache
            |> Seq.tryPick (fun (KeyValue(_, item)) ->
                if name = item.Data.Name then
                    Some item
                else
                    None
            )

type InventoryItem =
    {
        ItemId: ItemId
        Count: int
    }
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module InventoryItem =
    let init itemId count =
        {
            ItemId = itemId
            Count = count
        }

    let updateCount updating (inventoryItem: InventoryItem) =
        { inventoryItem with
            Count = updating inventoryItem.Count
        }

type Inventory = Map<ItemId, InventoryItem>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Inventory =
    let update itemId upd (inventory: Inventory): Inventory =
        match Map.tryFind itemId inventory with
        | Some inventoryItem ->
            let item = InventoryItem.updateCount upd inventoryItem
            if item.Count > 0 then
                Map.add itemId item inventory
            else
                Map.remove itemId inventory

        | None ->
            let item = InventoryItem.init itemId (upd 0)
            if item.Count > 0 then
                Map.add itemId item inventory
            else
                inventory

    let tryFindById itemId (inventory: Inventory) =
        Map.tryFind itemId inventory

module Players =
    type MainData =
        {
            Inventory: Inventory
            Catches: Set<ItemId>
        }
        static member Empty =
            {
                Inventory = Map.empty
                Catches = Set.empty
            }
        static member Serialize (data: MainData) =
            data |> Json.ser
        static member Deserialize json =
            try
                Ok (Json.des json)
            with e ->
                Error e.Message

    type Version =
        | V0 = 0

    type Player = CommonDb.Data<UserId, Version, MainData>
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Player =
        let create id data =
            CommonDb.Data.create id Version.V0 data

    type GuildData = CommonDb.GuildData<UserId, Version, MainData>
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module GuildData =
        let createData id =
            Player.create id MainData.Empty

        let init collectionName (db: IMongoDatabase): GuildData =
            MongoDB.Bson.Serialization.BsonSerializer.RegisterSerializer(typeof<Inventory>, new Db.MapSerializer<ItemId, InventoryItem>())

            CommonDb.GuildData.init
                createData
                (fun ver x ->
                    match Option.get ver with
                    | Version.V0 ->
                        None, Serialization.BsonSerializer.Deserialize<Player>(x)
                    | x ->
                        failwithf "Version = %A not implemented" x
                )
                collectionName
                db

        let set userId setAdditionParams (guildData: GuildData) =
            CommonDb.GuildData.set
                createData
                userId
                setAdditionParams
                guildData

        let drop (db: IMongoDatabase) (items: GuildData) =
            CommonDb.GuildData.drop db items

        let tryFindById id (items: GuildData): Player option =
            CommonDb.GuildData.tryFind id items

module Settings =
    type MainData =
        {
            BaseCatchId: ItemId option
        }
        static member Empty =
            {
                BaseCatchId = None
            }
        static member Serialize (data: MainData) =
            data |> Json.ser
        static member Deserialize json =
            try
                let res: MainData = Json.des json
                Ok res
            with e ->
                Error e.Message

    type Version =
        | V0 = 0

    type Id = unit

    type Data = CommonDb.Data<Id, Version, MainData>
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Data =
        let create id data =
            CommonDb.Data.create id Version.V0 data

    type GuildData = CommonDb.GuildData<Id, Version, MainData>
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module GuildData =
        let createData id =
            Data.create id MainData.Empty

        let init collectionName (db: IMongoDatabase): GuildData =
            CommonDb.GuildData.init
                createData
                (fun ver x ->
                    match Option.get ver with
                    | Version.V0 ->
                        None, Serialization.BsonSerializer.Deserialize<Data>(x)
                    | x ->
                        failwithf "Version = %A not implemented" x
                )
                collectionName
                db

        let set setAdditionParams (guildData: GuildData) =
            CommonDb.GuildData.set
                createData
                ()
                setAdditionParams
                guildData

        let drop (db: IMongoDatabase) (items: GuildData) =
            CommonDb.GuildData.drop db items

        let tryFindById id (items: GuildData) =
            CommonDb.GuildData.tryFind id items
