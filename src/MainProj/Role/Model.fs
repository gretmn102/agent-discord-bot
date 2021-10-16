module Role.Model
open MongoDB.Driver
open MongoDB.Bson

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
let database = client.GetDatabase("bot")

module Roles =
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

module PermissiveRoles =
    type PermissiveRolesData =
        {
            mutable Id: ObjectId
            mutable GuildId: GuildId
            mutable RoleIds: RoleId Set
        }
        static member Init(guildId: GuildId, roleIds: RoleId Set) =
            {
                Id = ObjectId.Empty
                GuildId = guildId
                RoleIds = roleIds
            }

    let permissiveRoles = database.GetCollection<PermissiveRolesData>("permissiveRoles")

    type GuildPermissiveRoles = Map<GuildId, PermissiveRolesData>

    let getAll (): GuildPermissiveRoles =
        permissiveRoles.Find(fun x -> true).ToEnumerable()
        |> Seq.fold
            (fun st x ->
                Map.add x.GuildId x st
            )
            Map.empty

    let replace (newData: PermissiveRolesData) =
        permissiveRoles.ReplaceOne((fun x -> x.Id = newData.Id), newData)
        |> ignore

    let insert (guildId: GuildId, roleIds: RoleId Set) =
        let x = PermissiveRolesData.Init(guildId, roleIds)
        permissiveRoles.InsertOne(x)
        x
