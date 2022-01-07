module Db
open MongoDB.Driver

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
let database = client.GetDatabase("bot")
