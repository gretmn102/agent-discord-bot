module DiscordWebhook.Model
open FsharpMyExtension
open MongoDB.Driver
open MongoDB.Bson

open Types

type Profile = { Username: string; AvatarUrl: string }
type Key = string

module Characters =
    type CharacterData =
        {
            mutable Id: ObjectId
            mutable GuildId: GuildId
            mutable UserId: UserId
            mutable Profiles: (Key * Profile) []
        }
        static member Init(guildId, userId, characters) =
            {
                Id = ObjectId.Empty
                GuildId = guildId
                UserId = userId
                Profiles = characters
            }

    let characters = Db.database.GetCollection<CharacterData>("characters")

    type GuildCharacters = Map<GuildId, Map<UserId, CharacterData>>

    let getAll (): GuildCharacters =
        characters.Find(fun x -> true).ToEnumerable()
        |> Seq.fold
            (fun st x ->
                st
                |> Map.addOrModWith
                    x.GuildId
                    (fun () -> Map.add x.UserId x Map.empty)
                    (fun st -> Map.add x.UserId x st)
            )
            Map.empty

    let replace (newData: CharacterData) =
        characters.ReplaceOne((fun x -> x.Id = newData.Id), newData)
        |> ignore

    let insert (guildId, userId, profiles) =
        let x = CharacterData.Init(guildId, userId, profiles)
        characters.InsertOne(x)
        x
