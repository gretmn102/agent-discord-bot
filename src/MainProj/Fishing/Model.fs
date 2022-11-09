module Fishing.Model
open FsharpMyExtension
open MongoDB.Driver
open MongoDB.Bson

open Types

type ItemId = System.Guid

module ItemsDb =
    type ItemData =
        {
            Name: string
            Loot: ItemId []
            Description: string
            ImageUrl: string
        }
        static member Create name loot description imageUrl =
            {
                Name = name
                Loot = loot
                Description = description
                ImageUrl = imageUrl
            }
        static member Empty =
            {
                Name = ""
                Loot = [||]
                Description = ""
                ImageUrl = ""
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

    type Item<'ItemData> =
        {
            Id: ItemId
            Version: Version
            Data: 'ItemData
        }

    /// Current version of item
    type ItemT = Item<ItemData>

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Item =
        let create id (data: 'ItemData) =
            {
                Id = id
                Version = Version.V0
                Data = data
            }

        let isBait (item: ItemT) =
            not (Array.isEmpty item.Data.Loot)

        let ofEditorItem (item: Editor.Types.Item): ItemT =
            let x =
                {
                    Name = item.Name
                    Loot = item.Loot
                    Description = item.Description
                    ImageUrl = item.ImageUrl
                }
            create item.ItemId x

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

    type Collection = IMongoCollection<BsonDocument>
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Collection =
        let replace (newData: ItemT) (collection: Collection) =
            let doc = newData.ToBsonDocument()

            let el = BsonElement("_id", BsonValue.Create(newData.Id))
            let i = new BsonDocument(el)

            let filter = FilterDefinition.op_Implicit(i)

            collection.ReplaceOne(filter, doc)
            |> ignore

        let insert itemId setAdditionParams (collection: Collection): Item<'ItemData> =
            let itemId =
                itemId
                |> Option.defaultWith (fun () ->
                    System.Guid.NewGuid()
                )

            let x = Item.create itemId (setAdditionParams ItemData.Empty)
            collection.InsertOne(x.ToBsonDocument())
            x

    type Items =
        {
            Cache: Map<ItemId, ItemT>
            Collection: Collection
        }

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Items =
        let collectionName = "fishingItems"

        let init (db: IMongoDatabase): Items =
            let collection = db.GetCollection<BsonDocument>(collectionName)

            if IMongoCollection.isEmpty collection then
                let item =
                    {
                        Cache = Map.empty
                        Collection = collection
                    }

                item
            else
                {
                    Cache =
                        collection.Find(fun x -> true).ToEnumerable()
                        |> Seq.fold
                            (fun st x ->
                                let ver =
                                    match x.["Version"] with
                                    | null -> failwithf "`Version` but\n%A" x
                                    | x ->
                                        if x.IsInt32 then
                                            enum<Version> x.AsInt32
                                        else
                                            failwithf "Version not int32 but %A" x
                                let x =
                                    match ver with
                                    | Version.V0 ->
                                        Serialization.BsonSerializer.Deserialize<ItemT>(x)
                                    | x ->
                                        failwithf "Version = %A not implemented" x

                                Map.add x.Id x st
                            )
                            Map.empty
                    Collection = collection
                }

        let set itemIdOpt setAdditionParams (items: Items) =
            let cache = items.Cache

            {
                items with
                    Cache =
                        let insertNew id =
                            let item =
                                items.Collection
                                |> Collection.insert id setAdditionParams

                            Map.add item.Id item cache

                        match itemIdOpt with
                        | Some itemId ->
                            match Map.tryFind itemId cache with
                            | Some item ->
                                let item =
                                    { item with
                                        Data = setAdditionParams item.Data
                                    }

                                Collection.replace item items.Collection

                                Map.add itemId item cache
                            | None ->
                                insertNew (Some itemId)
                        | None ->
                            insertNew None
            }

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
            db.DropCollection collectionName

            { items with
                Cache = Map.empty
            }

        let tryFindById id (items: Items) =
            Map.tryFind id items.Cache

        let tryFindByName name (items: Items) =
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
        {
            inventoryItem with Count = updating inventoryItem.Count
        }

type Inventory = Map<ItemId, InventoryItem>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Inventory =
    let update itemId upd (inventory: Inventory) =
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

    type Data<'MainData> =
        {
            Id: ObjectId
            UserId: UserId
            Version: Version
            Data: 'MainData
        }
        static member Init(data: 'MainData, userId: UserId) =
            {
                Id = ObjectId.Empty
                UserId = userId
                Version = Version.V0
                Data = data
            }

    type Collection = IMongoCollection<BsonDocument>

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Collection =
        let replace (newData: Data<MainData>) (collection: Collection) =
            let doc = newData.ToBsonDocument()

            let el = BsonElement("_id", BsonValue.Create(newData.Id))
            let i = new BsonDocument(el)

            let filter = FilterDefinition.op_Implicit(i)

            collection.ReplaceOne(filter, doc)
            |> ignore

        let insert userId setAdditionParams (collection: Collection) =
            let x =
                let data = Data.Init(MainData.Empty, userId)
                { data with
                    Data = setAdditionParams data.Data
                }

            let d = x.ToBsonDocument()
            collection.InsertOne(d)

            let newId = d.["_id"].AsObjectId

            { x with
                Id = newId
            }

    type GuildData =
        {
            Cache: Map<UserId, Data<MainData>>
            Collection: Collection
        }

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module GuildData =
        let set userId setAdditionParams (guildData: GuildData) =
            let cache = guildData.Cache

            {
                guildData with
                    Cache =
                        match Map.tryFind userId cache with
                        | Some fullData ->
                            let data =
                                { fullData with
                                    Data = setAdditionParams fullData.Data
                                }

                            Collection.replace data guildData.Collection

                            Map.add userId data cache
                        | None ->
                            let x =
                                guildData.Collection
                                |> Collection.insert userId setAdditionParams

                            Map.add userId x cache
            }

        let collectionName = "fishingPlayers"

        let init (db: IMongoDatabase): GuildData =
            MongoDB.Bson.Serialization.BsonSerializer.RegisterSerializer(typeof<Inventory>, new Db.MapSerializer<ItemId, InventoryItem>())

            let collection = db.GetCollection<BsonDocument>(collectionName)

            if IMongoCollection.isEmpty collection then
                {
                    Cache = Map.empty
                    Collection = collection
                }
            else
                {
                    Cache =
                        collection.Find(fun x -> true).ToEnumerable()
                        |> Seq.fold
                            (fun st x ->
                                let ver =
                                    match x.["Version"] with
                                    | null -> failwithf "`Version` but\n%A" x
                                    | x ->
                                        if x.IsInt32 then
                                            enum<Version> x.AsInt32
                                        else
                                            failwithf "Version not int32 but %A" x
                                let x =
                                    match ver with
                                    | Version.V0 ->
                                        Serialization.BsonSerializer.Deserialize<Data<MainData>>(x)
                                    | x ->
                                        failwithf "Version = %A not implemented" x

                                Map.add x.UserId x st
                            )
                            Map.empty
                    Collection = collection
                }

        let drop (db: IMongoDatabase) (guildWelcomeSetting: GuildData) =
            db.DropCollection collectionName

            { guildWelcomeSetting with
                Cache = Map.empty
            }

        let tryFind userId (guildWelcomeSetting: GuildData) =
            Map.tryFind userId guildWelcomeSetting.Cache
            |> Option.map (fun x -> x.Data)

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

    type Id = ObjectId

    type Data<'MainData> =
        {
            Id: Id
            Version: Version
            Data: 'MainData
        }
        static member Init(data: 'MainData) =
            {
                Id = ObjectId.Empty
                Version = Version.V0
                Data = data
            }

    type Collection = IMongoCollection<BsonDocument>

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Collection =
        let replace (newData: Data<MainData>) (collection: Collection) =
            let doc = newData.ToBsonDocument()

            let el = BsonElement("_id", BsonValue.Create(newData.Id))
            let i = new BsonDocument(el)

            let filter = FilterDefinition.op_Implicit(i)

            collection.ReplaceOne(filter, doc)
            |> ignore

        let insert setAdditionParams (collection: Collection) =
            let x =
                Data.Init(setAdditionParams MainData.Empty)

            let d = x.ToBsonDocument()
            collection.InsertOne(d)

            let newId = d.["_id"].AsObjectId

            { x with
                Id = newId
            }

    type GuildData =
        {
            Cache: Data<MainData>
            Collection: Collection
        }

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module GuildData =
        let collectionName = "fishingSettings"

        let init (db: IMongoDatabase): GuildData =
            let collection = db.GetCollection<BsonDocument>(collectionName)

            if IMongoCollection.isEmpty collection then
                {
                    Cache = Collection.insert id collection
                    Collection = collection
                }
            else
                {
                    Cache =
                        let res =
                            collection.Find(fun x -> true).ToEnumerable()
                            |> Seq.tryHead

                        match res with
                        | Some x ->
                            let ver =
                                match x.["Version"] with
                                | null -> failwithf "`Version` but\n%A" x
                                | x ->
                                    if x.IsInt32 then
                                        enum<Version> x.AsInt32
                                    else
                                        failwithf "Version not int32 but %A" x

                            match ver with
                            | Version.V0 ->
                                Serialization.BsonSerializer.Deserialize<Data<MainData>>(x)
                            | x ->
                                failwithf "Version = %A not implemented" x

                        | None ->
                            Collection.insert id collection

                    Collection = collection
                }

        let set setAdditionParams (guildData: GuildData) =
            let cache = guildData.Cache

            {
                guildData with
                    Cache =
                        let data =
                            { cache with
                                Data = setAdditionParams cache.Data
                            }

                        Collection.replace data guildData.Collection

                        data
            }

        let drop (db: IMongoDatabase) (settings: GuildData) =
            db.DropCollection collectionName

            { settings with
                Cache = Collection.insert id settings.Collection
            }

        let get (guildWelcomeSetting: GuildData) =
            guildWelcomeSetting.Cache
