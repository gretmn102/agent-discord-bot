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

let login = System.Environment.GetEnvironmentVariable "BotDbL"
let password = System.Environment.GetEnvironmentVariable "BotDbP"

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
        let varName = "DataBaseName"

        match getEnvironmentVariable varName with
        | Some value -> value
        | None ->
            failwithf "Environment variable `%s` not setup" varName

    client.GetDatabase(dataBaseName)

let superUserId = 796931597898088448UL
