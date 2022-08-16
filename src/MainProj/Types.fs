module Types
open FsharpMyExtension
open FsharpMyExtension.Either

module WebClientDownloader =
    open FsharpMyExtension.WebClientDownloader

    let getData (headers:(string * string) list) (url:string) =
        let hd = webClient.Headers

        headers |> List.iter hd.Set
        try
            webClient.DownloadData url
            |> Right
        with
            | x -> Left x.Message

type GuildId = uint64
type UserId = uint64
type ChannelId = uint64
type MessageId = uint64
type EmojiId = uint64
type RoleId = uint64
type WebhookId = uint64

[<Struct>]
type Snowflake =
    { v: uint64 }
    with
        static member Create v = { v = v }

type SnowflakeConverter() =
    inherit Newtonsoft.Json.JsonConverter<Snowflake>()

    override x.WriteJson(writer: Newtonsoft.Json.JsonWriter, value: Snowflake, serializer: Newtonsoft.Json.JsonSerializer) =
        writer.WriteValue(string value.v)

    override x.ReadJson(reader: Newtonsoft.Json.JsonReader, objectType: System.Type, existingValue: Snowflake, hasExistingValue: bool, serializer: Newtonsoft.Json.JsonSerializer): Snowflake =
        if hasExistingValue then
            existingValue
        else
            let res = reader.Value :?> string
            match System.UInt64.TryParse res with
            | true, v -> Snowflake.Create v
            | false, _ -> failwithf "snowflake parse error: %A" res

type NullableSnowflakeConverter() =
    inherit Newtonsoft.Json.JsonConverter<System.Nullable<Snowflake>>()

    override x.WriteJson(writer: Newtonsoft.Json.JsonWriter, value: System.Nullable<Snowflake>, serializer: Newtonsoft.Json.JsonSerializer) =
        if value.HasValue then
            writer.WriteValue(string value.Value.v)
        else
            writer.WriteValue(null: obj)

    override x.ReadJson(reader: Newtonsoft.Json.JsonReader, objectType: System.Type, existingValue: System.Nullable<Snowflake>, hasExistingValue: bool, serializer: Newtonsoft.Json.JsonSerializer): System.Nullable<Snowflake> =
        if hasExistingValue then
            existingValue
        else
            match reader.Value with
            | null -> System.Nullable()
            | :? string as v ->
                match System.UInt64.TryParse v with
                | true, v -> System.Nullable(Snowflake.Create v)
                | false, _ -> failwithf "snowflake parse error: %A" v
            | x -> failwithf "snowflake parse error: expected string but %A" x

type MessagePath =
    {
        GuildId: GuildId
        ChannelId: ChannelId
        MessageId: MessageId
    }
    static member OfDiscordMessage (msg: DSharpPlus.Entities.DiscordMessage) =
        {
            GuildId = msg.Channel.Guild.Id
            ChannelId = msg.Channel.Id
            MessageId = msg.Id
        }
    member this.ToDiscordPath =
        sprintf "https://discord.com/channels/%d/%d/%d" this.GuildId this.ChannelId this.MessageId

module StandartDiscordEmoji =
    let emojiSheetMapWidth = 42

    let emojiSheetMap =
        System.IO.File.ReadAllLines "DiscordEmojiSheet.txt"
        |> Array.mapi (fun y ->
            String.split " "
            >> Seq.mapi (fun x str -> str, (x, y))
            >> Array.ofSeq
        )
        |> Array.concat
        |> Map.ofSeq


    open SixLabors.ImageSharp
    open SixLabors.ImageSharp.PixelFormats
    open SixLabors.ImageSharp.Processing
    open SixLabors.ImageSharp.Drawing.Processing

    let emojiSheetUrl = "https://discord.com/assets/2071e22f8044e1f9e4f7c7afb7ac484a.png"
    let getEmojiSheet () =
        WebClientDownloader.getData [] emojiSheetUrl
        |> Either.map Image.Load

    let getEmoji (emojiSheet: Image<Rgba32>) (unicodeEmoji: string) (outputStream: System.IO.MemoryStream) =
        match Map.tryFind unicodeEmoji emojiSheetMap with
        | Some (x, y) ->
            let emojiSize = emojiSheet.Width / emojiSheetMapWidth
            use emoji =
                emojiSheet.Clone(fun ctx ->
                    let r = Rectangle(x * emojiSize, y * emojiSize, emojiSize, emojiSize)
                    ctx.Crop r
                    |> ignore
                )
            emoji.Save(outputStream, Formats.Png.PngFormat.Instance)

            true
        | None -> false


type ResultView =
    {
        View: DSharpPlus.Entities.DiscordMessageBuilder option
        ResponseToUser: DSharpPlus.Entities.DiscordMessageBuilder option
    }

open System.Threading.Tasks

let await (t:Task<_>) =
    t.GetAwaiter() |> fun x -> x.GetResult()
let awaiti (t:Task) =
    t.GetAwaiter().GetResult()

let getGuildMember (guild: DSharpPlus.Entities.DiscordGuild) (user: DSharpPlus.Entities.DiscordUser) =
    match user with
    | :? DSharpPlus.Entities.DiscordMember as guildMember -> guildMember
    | user -> await (guild.GetMemberAsync user.Id)

open dotenv.net

let getEnvironmentVariable =
    DotEnv.Load()

    let envVars = DotEnv.Read()

    fun envVar ->
        match envVars.TryGetValue envVar with
        | false, _ ->
            match System.Environment.GetEnvironmentVariable envVar with
            | null -> None
            | value -> Some value
        | true, value -> Some value
