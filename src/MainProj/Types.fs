module Types

type GuildId = uint64
type UserId = uint64
type ChannelId = uint64
type MessageId = uint64

type MessagePath =
    {
        GuildId: GuildId
        ChannelId: ChannelId
        MessageId: MessageId
    }

open System.Threading.Tasks

let await (t:Task<_>) =
    t.GetAwaiter() |> fun x -> x.GetResult()
let awaiti (t:Task<_>) =
    await t |> ignore
