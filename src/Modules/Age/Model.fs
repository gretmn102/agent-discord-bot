module Age.Model
open FsharpMyExtension
open MongoDB.Driver
open MongoDB.Bson

open Types

module Age =
    type Data =
        {
            mutable Id: ObjectId
            mutable UserId: UserId
            mutable Age: int
            mutable GuildIds: GuildId Set
        }
        static member Init(userId, date, guildIds) =
            {
                Id = ObjectId.Empty
                UserId = userId
                Age = date
                GuildIds = guildIds
            }

    let table = Db.database.GetCollection<Data>("age")

    type Users = Map<UserId, Data>

    let getAll (): Users =
        table.Find(fun x -> true).ToEnumerable()
        |> Seq.fold
            (fun st x ->
                Map.add x.UserId x st
            )
            Map.empty

    let replace (newData: Data) =
        table.ReplaceOne((fun x -> x.Id = newData.Id), newData)
        |> ignore

    let insert (guildId, roleId, guildIds) =
        let x = Data.Init(guildId, roleId, guildIds)
        table.InsertOne(x)
        x
