module Boosters.Model
open FsharpMyExtension
open MongoDB.Driver
open MongoDB.Bson

open Types

module Boosters =
    type SettingsData =
        {
            mutable Id: ObjectId
            mutable GuildId: GuildId
            mutable OutputChannelId: ChannelId
            mutable Message: string
        }
        static member Init(guildId, outputChannelId, message) =
            {
                Id = ObjectId.Empty
                GuildId = guildId
                OutputChannelId = outputChannelId
                Message = message
            }

    let settings = Db.database.GetCollection<SettingsData>("boosterGuildSettings")

    type GuildSettings = Map<GuildId, SettingsData>

    let getAll (): GuildSettings =
        settings.Find(fun x -> true).ToEnumerable()
        |> Seq.fold
            (fun st x ->
                Map.add x.GuildId x st
            )
            Map.empty

    let replace (newData: SettingsData) =
        settings.ReplaceOne((fun x -> x.Id = newData.Id), newData)
        |> ignore

    let insert (guildId, userId, profiles) =
        let x = SettingsData.Init(guildId, userId, profiles)
        settings.InsertOne(x)
        x
