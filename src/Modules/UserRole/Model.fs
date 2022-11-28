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

module PermissiveRolesOld =
    type PermissiveRolesData =
        {
            mutable Id: ObjectId
            mutable GuildId: GuildId
            mutable RoleIds: RoleId Set
        }
        static member Init(guildId: GuildId) =
            {
                Id = ObjectId.Empty
                GuildId = guildId
                RoleIds = Set.empty
            }

    type Collection = IMongoCollection<PermissiveRolesData>

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Collection =
        let replace (newData: PermissiveRolesData) (welcomeSetting: Collection) =
            welcomeSetting.ReplaceOne((fun x -> x.Id = newData.Id), newData)
            |> ignore

        let insert guildId setAdditionParams (welcomeSetting: Collection) =
            let x = setAdditionParams (PermissiveRolesData.Init guildId)
            welcomeSetting.InsertOne(x)
            x

    type GuildPermissiveRoles =
        {
            Cache: Map<GuildId, PermissiveRolesData>
            Collection: Collection
        }

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module GuildPermissiveRoles =
        let collectionName = "permissiveRoles"

        let getAll (db: IMongoDatabase): GuildPermissiveRoles =
            let welcomeSetting = db.GetCollection<PermissiveRolesData>(collectionName)

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

        let set guildId setAdditionParams (guildTemplateRoles: GuildPermissiveRoles) =
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

        let tryFind guildId (guildWelcomeSetting: GuildPermissiveRoles) =
            Map.tryFind guildId guildWelcomeSetting.Cache

        let drop (db: IMongoDatabase) (guildWelcomeSetting: GuildPermissiveRoles) =
            db.DropCollection collectionName

            { guildWelcomeSetting with
                Cache = Map.empty
            }

module TemplateRolesOld =
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
        let collectionName = "temlateRoles"

        let getAll (db: IMongoDatabase): GuildTemplateRoles =
            let welcomeSetting = db.GetCollection<TemplateRoleData>(collectionName)

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

        let drop (db: IMongoDatabase) (guildWelcomeSetting: GuildTemplateRoles) =
            db.DropCollection collectionName

            { guildWelcomeSetting with
                Cache = Map.empty
            }

module Setting =
    type Version =
        | V0 = 0
        | V1 = 1

    type MainDataV0 =
        {
            IsEnabled: bool
            TemplateRoleId: RoleId option
            PermissiveRoleIds: RoleId Set
        }
        static member Empty =
            {
                IsEnabled = true
                TemplateRoleId = None
                PermissiveRoleIds = Set.empty
            }

    type MainData =
        {
            IsEnabled: bool
            TemplateRoleId: RoleId option
            PermissiveRoleIds: RoleId Set
            PermissiveIconRoleIds: RoleId Set
        }
        static member Empty =
            {
                IsEnabled = true
                TemplateRoleId = None
                PermissiveRoleIds = Set.empty
                PermissiveIconRoleIds = Set.empty
            }

    type Data<'MainData> =
        {
            Id: ObjectId
            GuildId: GuildId
            Version: Version
            Data: 'MainData
        }
        static member Init(data: 'MainData, guildId: GuildId) =
            {
                Id = ObjectId.Empty
                GuildId = guildId
                Version = Version.V1
                Data = data
            }

    type Collection = IMongoCollection<BsonDocument>

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Collection =
        let replace (newData: Data<MainData>) (welcomeSetting: Collection) =
            let doc = newData.ToBsonDocument()

            let el = BsonElement("_id", BsonValue.Create(newData.Id))
            let i = new BsonDocument(el)

            let filter = FilterDefinition.op_Implicit(i)

            welcomeSetting.ReplaceOne(filter, doc)
            |> ignore

        let insert guildId setAdditionParams (collection: Collection) =
            let x =
                let data = Data.Init(MainData.Empty, guildId)
                { data with
                    Data = setAdditionParams data.Data
                }

            let d = x.ToBsonDocument()
            collection.InsertOne(d)

            let newId = d.["_id"].AsObjectId

            { x with
                Id = newId
            }

    type GuildData =
        {
            Cache: Map<GuildId, Data<MainData>>
            Collection: Collection
        }

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module GuildData =
        let set guildId setAdditionParams (guildData: GuildData) =
            let cache = guildData.Cache

            {
                guildData with
                    Cache =
                        match Map.tryFind guildId cache with
                        | Some fullData ->
                            let data =
                                { fullData with
                                    Data = setAdditionParams fullData.Data
                                }

                            Collection.replace data guildData.Collection

                            Map.add guildId data cache
                        | None ->
                            let x =
                                guildData.Collection
                                |> Collection.insert guildId setAdditionParams

                            Map.add guildId x cache
            }

        let collectionName = "userRoleSetting"

        let init (db: IMongoDatabase): GuildData =
            let collection = db.GetCollection<BsonDocument>(collectionName)

            if IMongoCollection.isEmpty collection then
                let guildData =
                    {
                        Cache = Map.empty
                        Collection = collection
                    }

                let permissiveRoles = PermissiveRolesOld.GuildPermissiveRoles.getAll db
                let guildData =
                    permissiveRoles.Collection.Find(fun _ -> true).ToEnumerable()
                    |> Seq.fold
                        (fun st x ->
                            st
                            |> set
                                x.GuildId
                                (fun data ->
                                    { data with
                                        PermissiveRoleIds = x.RoleIds
                                    }
                                )
                        )
                        guildData

                let templateRoles = TemplateRolesOld.GuildTemplateRoles.getAll db
                let guildData =
                    templateRoles.Collection.Find(fun _ -> true).ToEnumerable()
                    |> Seq.fold
                        (fun st x ->
                            st
                            |> set
                                x.GuildId
                                (fun data ->
                                    { data with
                                        TemplateRoleId = Some x.TemplateRoleId
                                    }
                                )
                        )
                        guildData

                PermissiveRolesOld.GuildPermissiveRoles.drop db permissiveRoles |> ignore
                TemplateRolesOld.GuildTemplateRoles.drop db templateRoles |> ignore

                guildData
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
                                    | Version.V1 ->
                                        Serialization.BsonSerializer.Deserialize<Data<MainData>>(x)
                                    | Version.V0 ->
                                        let x = Serialization.BsonSerializer.Deserialize<Data<MainDataV0>>(x)

                                        {
                                            Id = x.Id
                                            GuildId = x.GuildId
                                            Version = Version.V1
                                            Data =
                                                let x = x.Data
                                                {
                                                    IsEnabled = x.IsEnabled
                                                    TemplateRoleId = x.TemplateRoleId
                                                    PermissiveRoleIds = x.PermissiveRoleIds
                                                    PermissiveIconRoleIds = Set.empty
                                                }
                                        }
                                    | x ->
                                        failwithf "Version = %A not implemented" x

                                Map.add x.GuildId x st
                            )
                            Map.empty
                    Collection = collection
                }

        let drop (db: IMongoDatabase) (guildWelcomeSetting: GuildData) =
            db.DropCollection collectionName

            { guildWelcomeSetting with
                Cache = Map.empty
            }

        let tryFind guildId (guildWelcomeSetting: GuildData) =
            Map.tryFind guildId guildWelcomeSetting.Cache
            |> Option.map (fun x -> x.Data)