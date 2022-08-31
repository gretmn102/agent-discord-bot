module Db
open MongoDB.Driver

open Types

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
