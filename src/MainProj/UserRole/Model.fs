module UserRole.Model
open FsharpMyExtension
open MongoDB.Driver
open MongoDB.Bson

open Types
open Db

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

    type GuildUserRoles = Map<GuildId, Map<UserId, RoleData>>

    let getAll (): GuildUserRoles =
        roles.Find(fun x -> true).ToEnumerable()
        |> Seq.fold
            (fun st x ->
                st
                |> Map.addOrModWith
                    x.GuildId
                    (fun () -> Map.add x.UserId x Map.empty)
                    (fun st -> Map.add x.UserId x st)
            )
            Map.empty

    let replace (newRoleData: RoleData) =
        roles.ReplaceOne((fun x -> x.Id = newRoleData.Id), newRoleData)
        |> ignore

    let insert (guildId: GuildId, userId: UserId, roleId: RoleId) =
        let x = RoleData.Init(guildId, userId, roleId)
        roles.InsertOne(x)
        x

    let remove (roleData: RoleData) =
        roles.DeleteOne(fun x -> x.Id = roleData.Id)
        |> ignore

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

module TemplateRoles =
    type TemplateRoleData =
        {
            mutable Id: ObjectId
            mutable GuildId: GuildId
            mutable TemplateRoleId: RoleId
        }
        static member Init(guildId: GuildId) =
            {
                Id = ObjectId.Empty
                GuildId = guildId
                TemplateRoleId = 0UL
            }

    type Collection = IMongoCollection<TemplateRoleData>

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Collection =
        let replace (newData: TemplateRoleData) (welcomeSetting: Collection) =
            welcomeSetting.ReplaceOne((fun x -> x.Id = newData.Id), newData)
            |> ignore

        let insert guildId setAdditionParams (welcomeSetting: Collection) =
            let x = setAdditionParams (TemplateRoleData.Init guildId)
            welcomeSetting.InsertOne(x)
            x

    type GuildTemplateRoles =
        {
            Cache: Map<GuildId, TemplateRoleData>
            Collection: Collection
        }

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module GuildTemplateRoles =
        let getAll (db: IMongoDatabase): GuildTemplateRoles =
            let welcomeSetting = db.GetCollection<TemplateRoleData>("temlateRoles")

            {
                Cache =
                    welcomeSetting.Find(fun x -> true).ToEnumerable()
                    |> Seq.fold
                        (fun st x ->
                            Map.add x.GuildId x st
                        )
                        Map.empty
                Collection = welcomeSetting
            }

        let set guildId setAdditionParams (guildTemplateRoles: GuildTemplateRoles) =
            let cache = guildTemplateRoles.Cache

            {
                guildTemplateRoles with
                    Cache =
                        match Map.tryFind guildId cache with
                        | Some welcomeSettingData ->
                            let newcomersRoles =
                                setAdditionParams welcomeSettingData

                            Collection.replace newcomersRoles guildTemplateRoles.Collection

                            Map.add guildId newcomersRoles cache
                        | None ->
                            let x =
                                guildTemplateRoles.Collection
                                |> Collection.insert guildId setAdditionParams

                            Map.add guildId x cache
            }

        let tryFind guildId (guildWelcomeSetting: GuildTemplateRoles) =
            Map.tryFind guildId guildWelcomeSetting.Cache
