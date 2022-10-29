module Fishing.Model
open FsharpMyExtension
open MongoDB.Driver
open MongoDB.Bson

open Types

type ItemId = string

type Item =
    {
        ItemId: ItemId
        Name: string
        Loot: ItemId []
    }
    static member Create itemId name loot =
        {
            ItemId = itemId
            Name = name
            Loot = loot
        }

let withoutBait: Item =
    {
        ItemId = "0"
        Name = "Рыбачить без наживки"
        Loot = [||]
    }

type RawItems = list<string * string array>

let rawItems: RawItems =
    [("Ничего не подозревающий червячок",
      [|"Шоколадная пиявка"; "Плотвецкий рыба-кусь"|]);
     ("Шоколадная пиявка ",
      [|"Шоко-квакша земноводная"; "Дрожжевой линь"; "Рыбанан"; "Рыбаклажан"|]);
     ("Шоко-квакша земноводная", [|"Рваный французский флаг"|]);
     ("Дрожжевой линь",
      [|"Блиноподобная камбала "; "Рыбагет"; "Рыбатон";
        "Ромовая рыба (как баба, только рыба)"; "Куки-карп"|]);
     ("Блиноподобная камбала", [|"Тунец-шармец"|]);
     ("Рыбагет", [|"Круассалтус (круассан + палтус)"|]);
     ("Круассалтус ", [|"Рваный французский флаг"|]);
     ("Рыбатон", [|"Ржавое мукомольное сито"|]);
     ("Куки-карп", [|"Глубоководная печенюшная принцесса"; "Косяк крекер-плотвы"|]);
     ("Рыбанан", [|"Зефирный коралл"; "Медовый кои"|]);
     ("Медовый кои", [|"Рыба-пчела"|]); ("Рыба-пчела ", [|"Рыбопчлениый улей"|]);
     ("Рыбаклажан",
      [|"Сапог-саможуйка"; "Рыба-сэндвич (можно заменить на что-то интереснее)"|]);
     ("Рыба-сэндвич", [|"Лохнесское бутербродище"|]);
     ("Плотвецкий рыба-кусь",
      [|"Псевдовяленная вобла"; "Рыбутыль"; "СКАТерть";
        "Роговидная левозакрученная раковина с валерьянкой"; "Фуаг-рак"|]);
     ("Псевдовяленная вобла", [|"Рыба-Ёрш коктейлеобразная"|]);
     ("Рыба-Ёрш коктейлеобразная", [|"Невкусный ил"|]);
     ("Рыбутыль", [|"Волнорез-беконохвост"|]);
     ("Волнорез-беконохвост", [|"Глубоководный поильщик"|]);
     ("Глубоководный поильщик", [|"Хмель-водоросли"|]);
     ("СКАТерть", [|"Банка селёдочного варенья"|]);
     ("Банка селёдочного варенья ", [|"Рыбабушка склерозиус"|])]

let baseItemId = fst rawItems.[0]

let items: Map<ItemId, Item> =
    let fillEmptyItems (rawItems: RawItems) =
        let rawItems =
            rawItems
            |> List.map (fun (name, loot) -> name.Trim(), loot |> Array.map (fun x -> x.Trim()))

        let allItems =
            rawItems
            |> List.fold
                (fun st (_, loot) ->
                    loot |> Array.fold (fun st x -> Set.add x st) st
                )
                Set.empty
        let restItems =
            rawItems
            |> List.fold
                (fun st (name, _) -> Set.remove name st)
                allItems
        restItems
        |> Set.fold (fun st x -> (x, [||])::st) rawItems

    rawItems
    |> fillEmptyItems
    |> List.map (fun (name, loot) ->
        name, Item.Create name name loot
    )
    |> Map.ofList

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
        let inventoryItem =
            match Map.tryFind itemId inventory with
            | Some inventoryItem ->
                InventoryItem.updateCount upd inventoryItem
            | None ->
                InventoryItem.init itemId (upd 0)

        Map.add itemId inventoryItem inventory

MongoDB.Bson.Serialization.BsonSerializer.RegisterSerializer(typeof<Inventory>, new Db.MapSerializer<ItemId, InventoryItem>())

module Players =
    type MainData =
        {
            Inventory: Inventory
        }
        static member Empty =
            {
                Inventory = Map.empty
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
