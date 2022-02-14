module ReactionEvent.Model
open FsharpMyExtension
open MongoDB.Driver
open MongoDB.Bson

open Types

type Settings = Map<GuildId, RoleId []>

module ReactionEvents =
    type ReactionEventData =
        {
            mutable Id: ObjectId
            mutable GuildId: GuildId
            mutable ChannelId: ChannelId
            mutable MessageId: MessageId
            // if 0UL then it is a standard emoji
            mutable EmojiId: uint64
            mutable EmojiName: string
            mutable RoleIds: RoleId Set
        }
        static member Init (guildId, channelId, messageId, emojiId, emojiName, roleIds) =
            {
                Id = ObjectId.Empty
                GuildId = guildId
                ChannelId = channelId
                MessageId = messageId
                EmojiId = emojiId
                EmojiName = emojiName
                RoleIds = roleIds
            }

    let reactionEvents = Db.database.GetCollection<ReactionEventData>("reactionEvents")
    type Key =
        {
            ChannelId: ChannelId
            MessageId: MessageId
            // if 0UL then it is a standard emoji
            EmojiId: uint64
            EmojiName: string
        }
    type GuildReactionEvent = Map<GuildId, Map<Key, ReactionEventData>>

    let getAll (): GuildReactionEvent =
        reactionEvents.Find(fun x -> true).ToEnumerable()
        |> Seq.fold
            (fun st x ->
                let key = {
                    ChannelId = x.ChannelId
                    MessageId = x.MessageId
                    EmojiId = x.EmojiId
                    EmojiName = x.EmojiName
                }

                st
                |> Map.addOrModWith
                    x.GuildId
                    (fun () -> Map.add key x Map.empty)
                    (fun m -> Map.add key x m)
            )
            Map.empty

    let replace (newData: ReactionEventData) =
        reactionEvents.ReplaceOne((fun x -> x.Id = newData.Id), newData)
        |> ignore

    let insert (guildId, channelId, messageId, emojiId, emojiName, roleIds) =
        let x = ReactionEventData.Init(guildId, channelId, messageId, emojiId, emojiName, roleIds)
        reactionEvents.InsertOne(x)
        x
