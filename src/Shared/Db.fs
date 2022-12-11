module Db
open MongoDB.Driver
open MongoDB.Bson
open MongoDB.Bson.Serialization
open Newtonsoft.Json

open Types

type MapSerializer<'Key, 'Value when 'Key : comparison>() =
    inherit Serializers.SerializerBase<Map<'Key, 'Value>>()
    // https://stackoverflow.com/questions/47510650/c-sharp-mongodb-complex-class-serialization
    override __.Deserialize(context, args) =
        let serializer = BsonSerializer.LookupSerializer(typeof<BsonDocument>)
        let document = serializer.Deserialize(context, args)

        let bsonDocument = document.ToBsonDocument()

        let result = BsonExtensionMethods.ToJson(bsonDocument)
        JsonConvert.DeserializeObject<Map<'Key, 'Value>>(result)

    override __.Serialize(context, args, value) =
        let jsonDocument = JsonConvert.SerializeObject(value)
        let bsonDocument = BsonSerializer.Deserialize<BsonDocument>(jsonDocument)

        let serializer = BsonSerializer.LookupSerializer(typeof<BsonDocument>)
        serializer.Serialize(context, bsonDocument.AsBsonValue)

module CommonDb =
    type Data<'Id, 'Version, 'MainData when 'Version: enum<int>> =
        {
            Id: 'Id
            Version: 'Version
            Data: 'MainData
        }

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Data =
        let create (id: 'Id) (version: 'Version) (data: 'MainData) =
            {
                Id = id
                Version = version
                Data = data
            }

        let update updating (item: Data<'Id, 'Version, 'Data>): Data<'Id, 'Version, 'Data> =
            { item with
                Data = updating item.Data
            }

    type Collection = IMongoCollection<BsonDocument>

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Collection =
        let createFilterById (id: 'Id) =
            let convertIdToBsonElement (id: 'Id) =
                let value =
                    if Reflection.FSharpType.IsRecord typeof<'Id> then
                        id.ToBsonDocument() :> BsonValue
                    else
                        BsonValue.Create(id)

                BsonElement("_id", value)

            let el = convertIdToBsonElement id
            let i = new BsonDocument(el)

            FilterDefinition.op_Implicit(i)

        let mkReplace (newData: Data<'Id, 'Version, 'MainData>) =
            let doc = newData.ToBsonDocument()

            let filter = createFilterById newData.Id

            filter, doc

        let replace (newData: Data<'Id, 'Version, 'MainData>) (collection: Collection) =
            let filter, doc = mkReplace newData

            collection.ReplaceOne(filter, doc)
            |> ignore

        let insert createData id setAdditionParams (collection: Collection) =
            let x =
                let data = createData id
                { data with
                    Data = setAdditionParams data.Data
                }

            let d = x.ToBsonDocument()
            collection.InsertOne(d)

            x

        let bulkWrite (datas: (WriteModelType * Data<'Id, 'Version, 'MainData>) seq) (collection: Collection) =
            datas
            |> Seq.map (fun (writeModelType, newData) ->
                match writeModelType with
                | WriteModelType.ReplaceOne ->
                    let filter, doc = mkReplace newData

                    ReplaceOneModel(filter, doc) :> WriteModel<_>
                | WriteModelType.InsertOne ->
                    let doc = newData.ToBsonDocument()

                    InsertOneModel(doc) :> WriteModel<_>
                | _ ->
                    failwithf "%A write model not implemented!" writeModelType
            )
            |> collection.BulkWrite

        let removeById (id: 'Id) (collection: Collection) =
            let filter = createFilterById id
            collection.DeleteOne filter

        let removeByIds (ids: 'Id seq) (collection: Collection) =
            ids
            |> Seq.map (fun id ->
                let filter = createFilterById id
                DeleteOneModel(filter) :> WriteModel<_>
            )
            |> collection.BulkWrite

    type GuildData<'Id, 'Version, 'MainData when 'Id : comparison and 'Version: enum<int>> =
        {
            Cache: Map<'Id, Data<'Id, 'Version, 'MainData>>
            Collection: Collection
        }

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module GuildData =
        let inline init (createData: 'Id -> Data<'Id, ^Version, 'MainData>) convertToVersion collectionName (db: IMongoDatabase): GuildData<'Id, ^Version, 'MainData> =
            let collection = db.GetCollection<BsonDocument>(collectionName)

            if IMongoCollection.isEmpty collection then
                {
                    Cache = Map.empty
                    Collection = collection
                }
            else
                {
                    Cache =
                        let (deleteRequests, insertRequest), st =
                            collection.Find(fun x -> true).ToEnumerable()
                            |> Seq.fold
                                (fun ((deleteRequests, insertRequest) as request, st) doc ->
                                    let ver =
                                        // System.Collections.Generic.KeyNotFoundException: Element 'Version' not found.
                                        let version =
                                            try
                                                Some doc.["Version"]
                                            with
                                                | :? System.Collections.Generic.KeyNotFoundException ->
                                                    None

                                        version
                                        |> Option.map (fun x ->
                                            if x.IsInt32 then
                                                enum< ^Version> x.AsInt32
                                            else
                                                failwithf "Version not int32 but %A" x
                                        )

                                    let oldValueId, x =
                                        convertToVersion ver doc

                                    match oldValueId with
                                    | None ->
                                        request, Map.add x.Id x st
                                    | Some (id: 'OldId) ->
                                        match Map.tryFind x.Id st with // prevent collisions
                                        | None ->
                                            let insertRequest =
                                                (WriteModelType.InsertOne, x) :: insertRequest

                                            let deleteRequests =
                                                id :: deleteRequests

                                            (deleteRequests, insertRequest), Map.add x.Id x st
                                        | Some exists ->
                                            printfn "in db already exist value:\n%A\nbut:\n%A" exists x
                                            (deleteRequests, insertRequest), st
                                )
                                (([], []), Map.empty)

                        if not <| List.isEmpty deleteRequests then
                            let _ = Collection.removeByIds deleteRequests collection
                            ()

                        if not <| List.isEmpty insertRequest then
                            let _ = Collection.bulkWrite insertRequest collection
                            ()

                        st

                    Collection = collection
                }

        let set createData id setAdditionParams (guildData: GuildData<'Id, 'Version, 'MainData>) =
            let cache = guildData.Cache

            {
                guildData with
                    Cache =
                        match Map.tryFind id cache with
                        | Some fullData ->
                            let data =
                                { fullData with
                                    Data = setAdditionParams fullData.Data
                                }

                            Collection.replace data guildData.Collection

                            Map.add id data cache
                        | None ->
                            let x =
                                guildData.Collection
                                |> Collection.insert createData id setAdditionParams

                            Map.add id x cache
            }

        let sets (items: Data<'Id, 'Version, 'MainData> seq) (db: GuildData<'Id, 'Version, 'MainData>) =
            let cache = db.Cache
            let xs, cache =
                items
                |> Seq.mapFold
                    (fun cache item ->
                        let id: 'Id = item.Id
                        let writeModelType =
                            match Map.tryFind id cache with
                            | Some _ ->
                                WriteModelType.ReplaceOne

                            | None ->
                                WriteModelType.InsertOne

                        (writeModelType, item), Map.add id item cache
                    )
                    cache

            Collection.bulkWrite xs db.Collection |> ignore

            {
                db with
                    Cache = cache
            }


        let drop (db: IMongoDatabase) (guildWelcomeSetting: GuildData<'Id, 'Version, 'MainData>) =
            let collectionName = guildWelcomeSetting.Collection.CollectionNamespace.CollectionName
            db.DropCollection collectionName

            { guildWelcomeSetting with
                Cache = Map.empty
            }

        let tryFind id (guildWelcomeSetting: GuildData<'Id, 'Version, 'MainData>) =
            Map.tryFind id guildWelcomeSetting.Cache

        let removeById (id: 'Id) (db: GuildData<'Id, 'Version, 'MainData>) =
            let deleteResult =
                db.Collection
                |> Collection.removeById id

            let db =
                { db with
                    Cache = Map.remove id db.Cache
                }

            deleteResult, db

        let removeByIds (ids: 'Id seq) (db: GuildData<'Id, 'Version, 'MainData>) =
            let deleteResult =
                db.Collection
                |> Collection.removeByIds ids

            let db =
                { db with
                    Cache =
                        ids
                        |> Seq.fold (fun st x -> Map.remove x st) db.Cache
                }

            deleteResult, db

let superUserId = 796931597898088448UL
