module Role.Model
open MongoDB.Driver
open MongoDB.Bson

open Types

let login = System.Environment.GetEnvironmentVariable "BOT_DB_L"
let password = System.Environment.GetEnvironmentVariable "BOT_DB_P"

let settings =
    MongoClientSettings.FromConnectionString (
        sprintf
            "mongodb+srv://%s:%s@cluster0.jkwib.mongodb.net/myFirstDatabase?retryWrites=true&w=majority"
            login
            password
    )

let client = new MongoClient(settings)
let database = client.GetDatabase("bot")

type RoleData =
    {
        mutable Id: ObjectId
        mutable GuildId: GuildId
        mutable UserId: UserId
        mutable RoleId: RoleId
    }
    static member Init(guildId: GuildId, userId: UserId, roleId: RoleId) =
        {
            Id = ObjectId.Empty
            GuildId = guildId
            UserId = userId
            RoleId = roleId
        }

let roles = database.GetCollection<RoleData>("roles")

type Roles = Map<GuildId * UserId, RoleData>

let getAll (): Roles =
    roles.Find(fun x -> true).ToEnumerable()
    |> Seq.fold
        (fun st x ->
            Map.add (x.GuildId, x.UserId) x st
        )
        Map.empty

let replace (newRoleData: RoleData) =
    roles.ReplaceOne((fun x -> x.Id = newRoleData.Id), newRoleData)
    |> ignore

let insert (guildId: GuildId, userId: UserId, roleId: RoleId) =
    let x = RoleData.Init(guildId, userId, roleId)
    roles.InsertOne(x)
    x
