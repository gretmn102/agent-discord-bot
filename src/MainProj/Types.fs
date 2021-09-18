module Types

type GuildId = uint64
type UserId = uint64
type ChannelId = uint64
type MessageId = uint64
type EmojiId = uint64

type MessagePath =
    {
        GuildId: GuildId
        ChannelId: ChannelId
        MessageId: MessageId
    }

/// "🦉" -> Some "assets/59e611bd4994d2978d695df90db540c4.svg"
let unicodeEmojiGetUrlImage = function
    | "🦉" -> Some "assets/59e611bd4994d2978d695df90db540c4.svg"
    | "🥕" -> Some "assets/505b14bf36ec47cdbe22258a9419471e.svg"
    | _ -> None

type ResultView =
    {
        View: DSharpPlus.Entities.DiscordMessageBuilder option
        ResponseToUser: DSharpPlus.Entities.DiscordMessageBuilder option
    }

open System.Threading.Tasks

let await (t:Task<_>) =
    t.GetAwaiter() |> fun x -> x.GetResult()
let awaiti (t:Task<_>) =
    await t |> ignore
