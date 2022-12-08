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

    type RankingData =
        {
            mutable Id: ObjectId
            mutable GuildId: GuildId
            mutable UserId: UserId
            mutable Exp: Exp
            mutable DayExp: Exp
        }
        static member Init(guildId, userId, exp): RankingData =
            {
                Id = ObjectId.Empty
                GuildId = guildId
                UserId = userId
                Exp = exp
                DayExp = 0UL
            }

    let guildRankings = Db.database.GetCollection<RankingData>("guildRankings")

    type GuildRankings = Map<GuildId, Map<UserId, RankingData>>

    let getAll (): GuildRankings =
        guildRankings.Find(fun x -> true).ToEnumerable()
        |> Seq.fold
            (fun st x ->
                st
                |> Map.addOrModWith
                    x.GuildId
                    (fun () -> Map.add x.UserId x Map.empty)
                    (fun st -> Map.add x.UserId x st)
            )
            Map.empty

    let replace (newData: RankingData) =
        guildRankings.ReplaceOne((fun x -> x.Id = newData.Id), newData)
        |> ignore

    let insert (guildId, userId, exp) =
        let x = RankingData.Init(guildId, userId, exp)
        guildRankings.InsertOne(x)
        x

module MostActiveSettings =
    type Data =
        {
            mutable Id: ObjectId
            mutable GuildId: GuildId
            mutable MostActiveRoleId: RoleId
            mutable LastMostActiveUserId: UserId
            mutable LastUpdate: System.DateTime
        }
        static member Init(guildId, mostActiveRoleId, lastMostActiveUserId, lastUpdate): Data =
            {
                Id = ObjectId.Empty
                GuildId = guildId
                MostActiveRoleId = mostActiveRoleId
                LastMostActiveUserId = lastMostActiveUserId
                LastUpdate = lastUpdate
            }

    let collection = Db.database.GetCollection<Data>("mostActiveSettings")

    type GuildDatas = Map<GuildId, Data>

    let getAll (): GuildDatas =
        collection.Find(fun x -> true).ToEnumerable()
        |> Seq.fold
            (fun st x ->
                Map.add x.GuildId x st
            )
            Map.empty

    let replace (newData: Data) =
        collection.ReplaceOne((fun x -> x.Id = newData.Id), newData)
        |> ignore

    let insert (guildId, mostActiveRoleId, lastMostActiveUserId, lastUpdate) =
        let x = Data.Init(guildId, mostActiveRoleId, lastMostActiveUserId, lastUpdate)
        collection.InsertOne(x)
        x
