module Ranking.Model
open FsharpMyExtension
open MongoDB.Driver
open MongoDB.Bson

open Types

module RankingSettings =
    type LevelRole = { Role: RoleId; Level: int }

    type RankingSettingData =
        {
            mutable Id: ObjectId
            mutable GuildId: GuildId
            /// roles that are given at the specified level
            mutable LevelRoles: LevelRole []
            mutable OutputChannelId: ChannelId option
        }
        static member Init(guildId, roleIds, outputChannelId): RankingSettingData =
            {
                Id = ObjectId.Empty
                GuildId = guildId
                LevelRoles = roleIds
                OutputChannelId = outputChannelId
            }

    let guildRankingSettings = Db.database.GetCollection<RankingSettingData>("guildRankingSettings")

    type GuildRankingSettings = Map<GuildId, RankingSettingData>

    let getAll (): GuildRankingSettings =
        guildRankingSettings.Find(fun x -> true).ToEnumerable()
        |> Seq.fold
            (fun st x ->
                Map.add x.GuildId x st
            )
            Map.empty

    let replace (newData: RankingSettingData) =
        guildRankingSettings.ReplaceOne((fun x -> x.Id = newData.Id), newData)
        |> ignore

    let insert (guildId, roleIds, outputChannelId) =
        let x = RankingSettingData.Init(guildId, roleIds, outputChannelId)
        guildRankingSettings.InsertOne(x)
        x

module Rankings =
    type Exp = uint64

    type RankingData =
        {
            mutable Id: ObjectId
            mutable GuildId: GuildId
            mutable UserId: UserId
            mutable Exp: Exp
        }
        static member Init(guildId, userId, exp): RankingData =
            {
                Id = ObjectId.Empty
                GuildId = guildId
                UserId = userId
                Exp = exp
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
