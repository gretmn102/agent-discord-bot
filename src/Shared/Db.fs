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

    type Collection = IMongoCollection<BsonDocument>

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Collection =
        let replace (newData: Data<'Id, 'Version, 'MainData>) (collection: Collection) =
            let doc = newData.ToBsonDocument()

            let el = BsonElement("_id", BsonValue.Create(newData.Id))
            let i = new BsonDocument(el)

            let filter = FilterDefinition.op_Implicit(i)

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

        let removeById id (collection: Collection) =
            let el = BsonElement("_id", BsonValue.Create(id))
            let i = new BsonDocument(el)

            let filter = FilterDefinition.op_Implicit(i)

            collection.DeleteOne filter

    type GuildData<'Id, 'Version, 'MainData when 'Id : comparison and 'Version: enum<int>> =
        {
            Cache: Map<'Id, Data<'Id, 'Version, 'MainData>>
            Collection: Collection
        }

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module GuildData =
        let inline init createData convertToVersion collectionName (db: IMongoDatabase): GuildData<'Id, 'Version, 'MainData> =
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
                            (fun st doc ->
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
                                            enum<'Version> x.AsInt32
                                        else
                                            failwithf "Version not int32 but %A" x
                                    )

                                let oldValueId, x =
                                    convertToVersion ver doc

                                let x =
                                    match oldValueId with
                                    | None -> x
                                    | Some id ->
                                        collection
                                        |> Collection.removeById id
                                        |> ignore

                                        collection
                                        |> Collection.insert createData x.Id (fun _ -> x.Data)

                                Map.add x.Id x st
                            )
                            Map.empty
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

        let drop (db: IMongoDatabase) (guildWelcomeSetting: GuildData<'Id, 'Version, 'MainData>) =
            let collectionName = guildWelcomeSetting.Collection.CollectionNamespace.CollectionName
            db.DropCollection collectionName

            { guildWelcomeSetting with
                Cache = Map.empty
            }

        let tryFind id (guildWelcomeSetting: GuildData<'Id, 'Version, 'MainData>) =
            Map.tryFind id guildWelcomeSetting.Cache

let login = getEnvironmentVariable "BotDbL"
let password = getEnvironmentVariable "BotDbP"

let settings =
    MongoClientSettings.FromConnectionString (
        sprintf
            "mongodb+srv://%s:%s@cluster0.jkwib.mongodb.net/myFirstDatabase?retryWrites=true&w=majority"
            login
            password
    )

let client = new MongoClient(settings)
let database =
    let dataBaseName =
        getEnvironmentVariable "DataBaseName"

    client.GetDatabase(dataBaseName)

let superUserId = 796931597898088448UL
