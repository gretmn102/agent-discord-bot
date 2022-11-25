module Birthday.Model
open FsharpMyExtension
open MongoDB.Driver
open MongoDB.Bson

open Types

module BirthdaySetting =
    type CommandData =
        {
            RoleId: RoleId option
        }
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module CommandData =
        let empty =
            {
                RoleId = None
            }

        let create roleId =
            {
                RoleId = roleId
            }

        let serialize (data: CommandData) =
            data |> Json.ser

        let deserialize json =
            try
                let data: CommandData = Json.des json
                Ok data
            with e ->
                Error e.Message

    type Version =
        | V0 = 0

    type CommandId = GuildId

    type Command<'Data> =
        {
            Id: CommandId
            Version: Version
            Data: 'Data
        }
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Command =
        let create id (data: 'ItemData) =
            {
                Id = id
                Version = Version.V0
                Data = data
            }

    /// Current version of item
    type CommandT = Command<CommandData>

    type Collection = IMongoCollection<BsonDocument>
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Collection =
        let replace (newData: Command<'Data>) (collection: Collection) =
            let doc = newData.ToBsonDocument()

            let el = BsonElement("_id", BsonValue.Create(newData.Id))
            let i = new BsonDocument(el)

            let filter = FilterDefinition.op_Implicit(i)

            collection.ReplaceOne(filter, doc)
            |> ignore

        let insert id setAdditionParams (collection: Collection): Command<'ItemData> =
            let x = Command.create id (setAdditionParams CommandData.empty)
            collection.InsertOne(x.ToBsonDocument())
            x

    type Commands =
        {
            Cache: Map<CommandId, CommandT>
            Collection: Collection
        }

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Commands =
        let init (collectionName: string) (db: IMongoDatabase) : Commands =
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
                                        Serialization.BsonSerializer.Deserialize<CommandT>(x)
                                    | x ->
                                        failwithf "Version = %A not implemented" x

                                Map.add x.Id x st
                            )
                            Map.empty
                    Collection = collection
                }

        let set itemId setAdditionParams (items: Commands) =
            let cache = items.Cache

            {
                items with
                    Cache =
                        match Map.tryFind itemId cache with
                        | Some item ->
                            let item =
                                { item with
                                    Data = setAdditionParams item.Data
                                }

                            Collection.replace item items.Collection

                            Map.add itemId item cache
                        | None ->
                            let item =
                                items.Collection
                                |> Collection.insert itemId setAdditionParams

                            Map.add item.Id item cache
            }

        let drop (db: IMongoDatabase) (items: Commands) =
            db.DropCollection items.Collection.CollectionNamespace.CollectionName

            { items with
                Cache = Map.empty
            }

        let tryFindById id (items: Commands) =
            Map.tryFind id items.Cache

module Birthday =
    type DayMonth = { Day: int; Month: int }
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module DayMonth =
        let empty =
            {
                Day = 0
                Month = 0
            }

        let create day month =
            {
                Day = day
                Month = month
            }

    type CommandData =
        {
            Date: DayMonth
        }
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module CommandData =
        let empty =
            {
                Date = DayMonth.empty
            }

        let create date =
            {
                Date = date
            }

        let serialize (data: CommandData) =
            data |> Json.ser

        let deserialize json =
            try
                let data: CommandData = Json.des json
                Ok data
            with e ->
                Error e.Message

    type Version =
        | V0 = 0

    type CommandId = UserId

    type Command<'Data> =
        {
            Id: CommandId
            Version: Version
            Data: 'Data
        }
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Command =
        let create id (data: 'ItemData) =
            {
                Id = id
                Version = Version.V0
                Data = data
            }

    /// Current version of item
    type CommandT = Command<CommandData>

    type Collection = IMongoCollection<BsonDocument>
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Collection =
        let replace (newData: Command<'Data>) (collection: Collection) =
            let doc = newData.ToBsonDocument()

            let el = BsonElement("_id", BsonValue.Create(newData.Id))
            let i = new BsonDocument(el)

            let filter = FilterDefinition.op_Implicit(i)

            collection.ReplaceOne(filter, doc)
            |> ignore

        let insert id setAdditionParams (collection: Collection): Command<'ItemData> =
            let x = Command.create id (setAdditionParams CommandData.empty)
            collection.InsertOne(x.ToBsonDocument())
            x

    type Commands =
        {
            Cache: Map<CommandId, CommandT>
            Collection: Collection
        }

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Commands =
        let init (collectionName: string) (db: IMongoDatabase) : Commands =
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
                                        Serialization.BsonSerializer.Deserialize<CommandT>(x)
                                    | x ->
                                        failwithf "Version = %A not implemented" x

                                Map.add x.Id x st
                            )
                            Map.empty
                    Collection = collection
                }

        let set itemId setAdditionParams (items: Commands) =
            let cache = items.Cache

            {
                items with
                    Cache =
                        match Map.tryFind itemId cache with
                        | Some item ->
                            let item =
                                { item with
                                    Data = setAdditionParams item.Data
                                }

                            Collection.replace item items.Collection

                            Map.add itemId item cache
                        | None ->
                            let item =
                                items.Collection
                                |> Collection.insert itemId setAdditionParams

                            Map.add item.Id item cache
            }

        let drop (db: IMongoDatabase) (items: Commands) =
            db.DropCollection items.Collection.CollectionNamespace.CollectionName

            { items with
                Cache = Map.empty
            }

        let tryFindById id (items: Commands) =
            Map.tryFind id items.Cache
