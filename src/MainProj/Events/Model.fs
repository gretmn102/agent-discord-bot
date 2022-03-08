module Events.Model
open FsharpMyExtension
open MongoDB.Driver
open MongoDB.Bson

open Types

module WomensDaySetting =
    type WomensDaySettingData =
        {
            mutable Id: ObjectId
            mutable GuildId: GuildId
            mutable FilteringRoleId: RoleId
            mutable IsEnabled: bool
        }
        static member Init(guildId, filteringRoleId, isEnabled) =
            {
                Id = ObjectId.Empty
                GuildId = guildId
                FilteringRoleId = filteringRoleId
                IsEnabled = isEnabled
            }

    let table = Db.database.GetCollection<WomensDaySettingData>("womensDaySetting")

    type GuildWomensDaySetting = Map<GuildId, WomensDaySettingData>

    let getAll (): GuildWomensDaySetting =
        table.Find(fun x -> true).ToEnumerable()
        |> Seq.fold
            (fun st x ->
                Map.add x.GuildId x st
            )
            Map.empty

    let replace (newData: WomensDaySettingData) =
        table.ReplaceOne((fun x -> x.Id = newData.Id), newData)
        |> ignore

    let insert (guildId, filteringRoleId, isEnabled) =
        let x = WomensDaySettingData.Init(guildId, filteringRoleId, isEnabled)
        table.InsertOne(x)
        x
