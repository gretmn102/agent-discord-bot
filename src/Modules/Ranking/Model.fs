module Ranking.Model
open FsharpMyExtension
open MongoDB.Driver
open MongoDB.Bson
open MongoDB.Bson.Serialization.Attributes

open Types
open Db

module RankingSettings =
    type LevelRole = { Role: RoleId; Level: int }

    [<BsonIgnoreExtraElements>]
    type DataPreVersion =
        {
            Id: ObjectId
            GuildId: GuildId
            /// roles that are given at the specified level
            LevelRoles: LevelRole []
            OutputChannelId: ChannelId option
        }

    type GuildData =
        {
            /// roles that are given at the specified level
            LevelRoles: LevelRole []
            OutputChannelId: ChannelId option
        }
        static member Init levelRoles outputChannelId =
            {
                LevelRoles = levelRoles
                OutputChannelId = outputChannelId
            }
        static member Empty =
            {
                LevelRoles = [||]
                OutputChannelId = None
            }
        static member Serialize (data: GuildData) =
            data |> Json.ser
        static member Deserialize json =
            try
                Ok (Json.des json)
            with e ->
                Error e.Message

    type Version =
        | V0 = 0

    type Id = GuildId

    type Guild = CommonDb.Data<Id, Version, GuildData>
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Guild =
        let create id data: Guild =
            CommonDb.Data.create id Version.V0 data

    type Guilds = CommonDb.GuildData<Id, Version, GuildData>
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Guilds =
        let createData id =
            Guild.create id GuildData.Empty

        let init collectionName (db: IMongoDatabase): Guilds =
            CommonDb.GuildData.init
                createData
                (fun ver doc ->
                    match ver with
                    | Some ver ->
                        match ver with
                        | Version.V0 ->
                            None, Serialization.BsonSerializer.Deserialize<Guild>(doc)
                        | x ->
                            failwithf "Version = %A not implemented" x
                    | None ->
                        let oldValue =
                            Serialization.BsonSerializer.Deserialize<DataPreVersion>(doc)
                        let newValue =
                            GuildData.Init oldValue.LevelRoles oldValue.OutputChannelId
                            |> Guild.create oldValue.GuildId

                        Some oldValue.Id, newValue
                )
                collectionName
                db

        let set userId setAdditionParams (guildData: Guilds) =
            CommonDb.GuildData.set
                createData
                userId
                setAdditionParams
                guildData

        let drop (db: IMongoDatabase) (items: Guilds) =
            CommonDb.GuildData.drop db items

        let tryFindById id (items: Guilds): Guild option =
            CommonDb.GuildData.tryFind id items

module Rankings =
    type Exp = uint64

    [<BsonIgnoreExtraElements>]
    type DataPreVersion =
        {
            Id: ObjectId
            GuildId: GuildId
            UserId: UserId
            Exp: Exp
            DayExp: Exp
        }

    type GuildUserData =
        {
            Exp: Exp
            DayExp: Exp
        }
        static member Init exp dayExp =
            {
                Exp = exp
                DayExp = dayExp
            }
        static member Empty =
            {
                Exp = 0UL
                DayExp = 0UL
            }
        static member Serialize (data: GuildUserData) =
            data |> Json.ser
        static member Deserialize json =
            try
                Ok (Json.des json)
            with e ->
                Error e.Message

    type Version =
        | V0 = 0

    type Id =
        {
            GuildId: GuildId
            UserId: UserId
        }
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Id =
        let create guildId userId =
            {
                GuildId = guildId
                UserId = userId
            }

    type GuildUser = CommonDb.Data<Id, Version, GuildUserData>
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module GuildUser =
        let create id data: GuildUser =
            CommonDb.Data.create id Version.V0 data

        let update updating (guildUser: GuildUser): GuildUser =
            CommonDb.Data.update updating guildUser

    type GuildReactionEvent = Map<GuildId, UserId Set>
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module GuildReactionEvent =
        let set id (cache: GuildReactionEvent) =
            let guildMemberId = id.UserId

            let add =
                Set.add guildMemberId

            cache
            |> Map.addOrModWith
                id.GuildId
                (fun () -> add Set.empty)
                add

    type GuildUsers =
        {
            Db: CommonDb.GuildData<Id, Version, GuildUserData>
            Cache: GuildReactionEvent
        }
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module GuildUsers =
        let createData id =
            GuildUser.create id GuildUserData.Empty

        let init collectionName (db: IMongoDatabase): GuildUsers =
            let db =
                CommonDb.GuildData.init
                    createData
                    (fun ver doc ->
                        match ver with
                        | Some ver ->
                            match ver with
                            | Version.V0 ->
                                None, Serialization.BsonSerializer.Deserialize<GuildUser>(doc)
                            | x ->
                                failwithf "Version = %A not implemented" x
                        | None ->
                            let oldValue =
                                Serialization.BsonSerializer.Deserialize<DataPreVersion>(doc)
                            let newValue =
                                GuildUserData.Init oldValue.Exp oldValue.DayExp
                                |> GuildUser.create (Id.create oldValue.GuildId oldValue.UserId)

                            Some oldValue.Id, newValue
                    )
                    collectionName
                    db
            {
                Db = db
                Cache =
                    db.Cache
                    |> Map.fold
                        (fun cache id _ ->
                            GuildReactionEvent.set id cache
                        )
                        Map.empty
            }

        let set id setAdditionParams (guildData: GuildUsers) =
            {
                Db =
                    CommonDb.GuildData.set
                        createData
                        id
                        setAdditionParams
                        guildData.Db
                Cache =
                    GuildReactionEvent.set id guildData.Cache
            }

        let sets (ids: GuildUser seq) (guildData: GuildUsers) =
            {
                Db =
                    CommonDb.GuildData.sets
                        ids
                        guildData.Db
                Cache =
                    ids
                    |> Seq.fold
                        (fun cache guildUser -> GuildReactionEvent.set guildUser.Id cache)
                        guildData.Cache
            }

        let drop (db: IMongoDatabase) (items: GuildUsers) =
            {
                Db = CommonDb.GuildData.drop db items.Db
                Cache = Map.empty
            }

        let tryFindById id (items: GuildUsers): GuildUser option =
            CommonDb.GuildData.tryFind id items.Db

        let tryFindGuildUsers guildId (db: GuildUsers): Option<GuildUser []> =
            Map.tryFind guildId db.Cache
            |> Option.map (
                Seq.choose (fun userId ->
                    let id = Id.create guildId userId
                    match tryFindById id db with
                    | Some x -> Some x
                    | None ->
                        printfn "expected find %d userId in Rankings.GuildUsers from cache but not found" userId
                        None
                )
                >> Array.ofSeq
            )

module MostActiveSettings =
    [<BsonIgnoreExtraElements>]
    type DataPreVersion =
        {
            Id: ObjectId
            GuildId: GuildId
            MostActiveRoleId: RoleId
            LastMostActiveUserId: UserId
            LastUpdate: System.DateTime
        }

    type GuildData =
        {
            MostActiveRoleId: RoleId
            LastMostActiveUserId: UserId
            LastUpdate: System.DateTime
        }
        static member Init mostActiveRoleId lastMostActiveUserId lastUpdate =
            {
                MostActiveRoleId = mostActiveRoleId
                LastMostActiveUserId = lastMostActiveUserId
                LastUpdate = lastUpdate
            }
        static member Empty =
            {
                MostActiveRoleId = 0UL
                LastMostActiveUserId = 0UL
                LastUpdate = System.DateTime.MinValue
            }
        static member Serialize (data: GuildData) =
            data |> Json.ser
        static member Deserialize json =
            try
                Ok (Json.des json)
            with e ->
                Error e.Message

    type Version =
        | V0 = 0

    type Id = GuildId

    type Guild = CommonDb.Data<Id, Version, GuildData>
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Guild =
        let create id data: Guild =
            CommonDb.Data.create id Version.V0 data

    type Guilds = CommonDb.GuildData<Id, Version, GuildData>
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Guilds =
        let createData id =
            Guild.create id GuildData.Empty

        let init collectionName (db: IMongoDatabase): Guilds =
            CommonDb.GuildData.init
                createData
                (fun ver doc ->
                    match ver with
                    | Some ver ->
                        match ver with
                        | Version.V0 ->
                            None, Serialization.BsonSerializer.Deserialize<Guild>(doc)
                        | x ->
                            failwithf "Version = %A not implemented" x
                    | None ->
                        let oldValue =
                            Serialization.BsonSerializer.Deserialize<DataPreVersion>(doc)
                        let newValue =
                            GuildData.Init oldValue.MostActiveRoleId oldValue.LastMostActiveUserId oldValue.LastUpdate
                            |> Guild.create oldValue.GuildId

                        Some oldValue.Id, newValue
                )
                collectionName
                db

        let set userId setAdditionParams (guildData: Guilds) =
            CommonDb.GuildData.set
                createData
                userId
                setAdditionParams
                guildData

        let drop (db: IMongoDatabase) (items: Guilds) =
            CommonDb.GuildData.drop db items

        let tryFindById id (items: Guilds): Guild option =
            CommonDb.GuildData.tryFind id items
