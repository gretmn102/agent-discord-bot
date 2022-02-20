module Birthday.Model
open FsharpMyExtension
open MongoDB.Driver
open MongoDB.Bson

open Types

module BirthdaySetting =
    type BirthdaySettingData =
        {
            mutable Id: ObjectId
            mutable GuildId: GuildId
            mutable RoleId: RoleId
        }
        static member Init(guildId, roleId) =
            {
                Id = ObjectId.Empty
                GuildId = guildId
                RoleId = roleId
            }

    let birthdaySettings = Db.database.GetCollection<BirthdaySettingData>("birthdaySetting")

    type GuildBirthdaySetting = Map<GuildId, BirthdaySettingData>

    let getAll (): GuildBirthdaySetting =
        birthdaySettings.Find(fun x -> true).ToEnumerable()
        |> Seq.fold
            (fun st x ->
                Map.add x.GuildId x st
            )
            Map.empty

    let replace (newData: BirthdaySettingData) =
        birthdaySettings.ReplaceOne((fun x -> x.Id = newData.Id), newData)
        |> ignore

    let insert (guildId, roleId) =
        let x = BirthdaySettingData.Init(guildId, roleId)
        birthdaySettings.InsertOne(x)
        x

module Birthday =
    type DayMonth = { Day: int; Month: int }

    type BirthdayData =
        {
            mutable Id: ObjectId
            mutable UserId: UserId
            mutable Date: DayMonth
        }
        static member Init(userId, date) =
            {
                Id = ObjectId.Empty
                UserId = userId
                Date = date
            }

    let birthday = Db.database.GetCollection<BirthdayData>("birthday")

    type UsersBirthday = Map<UserId, BirthdayData>

    let getAll (): UsersBirthday =
        birthday.Find(fun x -> true).ToEnumerable()
        |> Seq.fold
            (fun st x ->
                Map.add x.UserId x st
            )
            Map.empty

    let replace (newData: BirthdayData) =
        birthday.ReplaceOne((fun x -> x.Id = newData.Id), newData)
        |> ignore

    let insert (guildId, roleId) =
        let x = BirthdayData.Init(guildId, roleId)
        birthday.InsertOne(x)
        x
