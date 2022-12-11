module Types
open FsharpMyExtension
open FsharpMyExtension.Either

type GuildId = uint64
type UserId = uint64
type ChannelId = uint64
type MessageId = uint64
type EmojiId = uint64
type RoleId = uint64
type WebhookId = uint64

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

type EnabledOptionValue<'Value> =
    {
        IsEnabled: bool
        Value: 'Value option
    }
    static member Empty: EnabledOptionValue<'Value> =
        {
            IsEnabled = false
            Value = None
        }
    static member Init (v: 'Value) =
        {
            IsEnabled = true
            Value = Some v
        }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module EnabledOptionValue =
    let fold folder (st: 'State) (v: EnabledOptionValue<'Value>) =
        if v.IsEnabled then
            match v.Value with
            | Some x -> folder x
            | None -> st
        else
            st

    let map mapping (v: EnabledOptionValue<'a>): EnabledOptionValue<'b> =
        match v.Value with
        | Some oldValue ->
            {
                IsEnabled = v.IsEnabled
                Value = Some (mapping oldValue)
            }
        | None ->
            {
                IsEnabled = v.IsEnabled
                Value = None
            }

    let toOption (v: EnabledOptionValue<'a>): 'a option =
        if v.IsEnabled then
            v.Value
        else
            None

module StandartDiscordEmoji =
    let emojiSheetMapWidth = 42

    let emojiSheetMap =
        System.IO.File.ReadAllLines "Shared/DiscordEmojiSheet.txt"
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

let tryGetEnvironmentVariable =
    DotEnv.Load()

    let envVars = DotEnv.Read()

    fun envVar ->
        match envVars.TryGetValue envVar with
        | false, _ ->
            match System.Environment.GetEnvironmentVariable envVar with
            | null -> None
            | value -> Some value
        | true, value -> Some value

let getEnvironmentVariable varName =
    tryGetEnvironmentVariable varName
    |> Option.defaultWith (fun () ->
        failwithf "Environment variable `%s` is not set!" varName
    )

open MongoDB.Driver

module IMongoCollection =
    let isEmpty (collection: IMongoCollection<_>) =
        let countOptions =
            CountOptions(
                Limit = System.Nullable(1L)
            )

        0L = collection.CountDocuments((fun _ -> true), countOptions)
