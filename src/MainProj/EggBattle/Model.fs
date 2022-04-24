module EggBattle.Model
open FsharpMyExtension
open MongoDB.Driver
open MongoDB.Bson

open Types

module Rating =
    type Data =
        {
            mutable Id: ObjectId
            mutable GuildId: GuildId
            mutable UserId: UserId
            mutable Wins: int
            mutable Loses: int
        }
        static member Init(guildId, userId, wins, loses) =
            {
                Id = ObjectId.Empty
                GuildId = guildId
                UserId = userId
                Wins = wins
                Loses = loses
            }

    let table = Db.database.GetCollection<Data>("EggBattleRatings")

    type GuildsRating = Map<GuildId, Map<UserId, Data>>

    let getAll (): GuildsRating =
        table.Find(fun x -> true).ToEnumerable()
        |> Seq.fold
            (fun st x ->
                st
                |> Map.addOrModWith
                    x.GuildId
                    (fun () -> Map.add x.UserId x Map.empty)
                    (fun st -> Map.add x.UserId x st)
            )
            Map.empty

    let replace (newData: Data) =
        table.ReplaceOne((fun x -> x.Id = newData.Id), newData)
        |> ignore

    let insert (guildId, userId, wins, loses) =
        let x = Data.Init(guildId, userId, wins, loses)
        table.InsertOne(x)
        x
