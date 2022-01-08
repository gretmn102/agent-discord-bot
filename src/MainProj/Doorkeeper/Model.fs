module Doorkeeper.Model
open FsharpMyExtension
open MongoDB.Driver
open MongoDB.Bson

open Types

type Settings = Map<GuildId, RoleId []>

module NewcomersRoles =
    type NewcomersRolesData =
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

    let permissiveRoles = Db.database.GetCollection<NewcomersRolesData>("newcomersRoles")

    type GuildNewcomersRoles = Map<GuildId, NewcomersRolesData>

    let getAll (): GuildNewcomersRoles =
        permissiveRoles.Find(fun x -> true).ToEnumerable()
        |> Seq.fold
            (fun st x ->
                Map.add x.GuildId x st
            )
            Map.empty

    let replace (newData: NewcomersRolesData) =
        permissiveRoles.ReplaceOne((fun x -> x.Id = newData.Id), newData)
        |> ignore

    let insert (guildId: GuildId, roleIds: RoleId Set) =
        let x = NewcomersRolesData.Init(guildId, roleIds)
        permissiveRoles.InsertOne(x)
        x
