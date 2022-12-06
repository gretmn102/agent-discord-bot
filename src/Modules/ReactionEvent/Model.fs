module ReactionEvent.Model
open FsharpMyExtension
open MongoDB.Driver
open MongoDB.Bson
open MongoDB.Bson.Serialization.Attributes

open Types
open Db

type Settings = Map<GuildId, RoleId []>

[<BsonIgnoreExtraElements>]
type DataPreVersion =
    {
        Id: ObjectId
        GuildId: GuildId
        ChannelId: ChannelId
        MessageId: MessageId
        // if 0UL then it is a standard emoji
        EmojiId: uint64
        EmojiName: string
        RoleIds: RoleId Set
    }

type MessagePath =
    {
        ChannelId: ChannelId
        MessageId: MessageId
    }
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module MessagePath =
    let create channelId messageId =
        {
            ChannelId = channelId
            MessageId = messageId
        }

type Emoji =
    {
        /// if 0UL then it is a standard emoji
        EmojiId: uint64
        EmojiName: string
    }
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Emoji =
    let create emojiId emojiName =
        {
            EmojiId = emojiId
            EmojiName = emojiName
        }

    let ofUnicodeOrCustomEmoji (emoji: DiscordMessage.UnicodeOrCustomEmoji) =
        {
            EmojiId =
                match emoji with
                | DiscordMessage.CustomEmoji e -> e.Id
                | DiscordMessage.UnicodeEmoji _ -> 0UL
            EmojiName =
                match emoji with
                | DiscordMessage.CustomEmoji e -> e.Name
                | DiscordMessage.UnicodeEmoji name -> name
        }

type GuildData =
    {
        RoleIds: RoleId Set
    }
    static member Init roleIds =
        {
            RoleIds = roleIds
        }
    static member Empty =
        {
            RoleIds = Set.empty
        }
    static member Serialize (data: GuildData) =
        data |> Json.ser
    static member Deserialize json =
        try
            Ok (Json.des json)
        with e ->
            Error e.Message

type Version =
    | V0 = 0

type Id =
    {
        GuildId: GuildId
        MessagePath: MessagePath
        Emoji: Emoji
    }
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Id =
    let create guildId messagePath emoji =
        {
            GuildId = guildId
            MessagePath = messagePath
            Emoji = emoji
        }

type Guild = CommonDb.Data<Id, Version, GuildData>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Guild =
    let create id data: Guild =
        CommonDb.Data.create id Version.V0 data

type GuildReactionEvent = Map<GuildId, Map<MessagePath, Emoji Set>>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module GuildReactionEvent =
    let set id (cache: GuildReactionEvent) =
        let messagePath = id.MessagePath

        let emoji = id.Emoji

        let add =
            Map.addOrModWith
                messagePath
                (fun () -> Set.add emoji Set.empty)
                (Set.add emoji)

        cache
        |> Map.addOrModWith
            id.GuildId
            (fun () -> add Map.empty)
            add

type Guilds =
    {
        Db: CommonDb.GuildData<Id, Version, GuildData>
        Cache: GuildReactionEvent
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Guilds =
    let createData id =
        Guild.create id GuildData.Empty

    let init collectionName (db: IMongoDatabase): Guilds =
        let db =
            CommonDb.GuildData.init
                createData
                (fun ver doc ->
                    match ver with
                    | Some ver ->
                        match ver with
                        | Version.V0 ->
                            None, Serialization.BsonSerializer.Deserialize<Guild>(doc)
                        | x ->
                            failwithf "Version = %A not implemented" x
                    | None ->
                        let oldValue =
                            Serialization.BsonSerializer.Deserialize<DataPreVersion>(doc)
                        let newValue =
                            let id =
                                Id.create
                                    oldValue.GuildId
                                    (MessagePath.create oldValue.ChannelId oldValue.MessageId)
                                    (Emoji.create oldValue.EmojiId oldValue.EmojiName)

                            GuildData.Init oldValue.RoleIds
                            |> Guild.create id

                        Some oldValue.Id, newValue
                )
                collectionName
                db
        {
            Db = db
            Cache =
                db.Cache
                |> Map.fold
                    (fun cache id item ->
                        GuildReactionEvent.set id cache
                    )
                    Map.empty
        }

    let set id setAdditionParams (guildData: Guilds) =
        {
            Db =
                CommonDb.GuildData.set
                    createData
                    id
                    setAdditionParams
                    guildData.Db
            Cache =
                GuildReactionEvent.set id guildData.Cache
        }

    let drop (db: IMongoDatabase) (items: Guilds) =
        {
            Db = CommonDb.GuildData.drop db items.Db
            Cache = Map.empty
        }

    let tryFindById id (items: Guilds): Guild option =
        CommonDb.GuildData.tryFind id items.Db

    let removeByGuildIdAndMessagePath (guildId: GuildId) (messagePath: MessagePath) (db: Guilds) =
        match Map.tryFind guildId db.Cache with
        | Some messagePaths ->
            match Map.tryFind messagePath messagePaths with
            | Some reactionEvents ->
                let ids =
                    reactionEvents
                    |> Seq.map (fun x ->
                        Id.create
                            guildId
                            messagePath
                            x
                    )
                let db =
                    {
                        Db =
                            let _, db = CommonDb.GuildData.removeByIds ids db.Db
                            db
                        Cache =
                            let x = Map.remove messagePath messagePaths
                            Map.add guildId x db.Cache
                    }

                true, db

            | None ->
                false, db
        | None ->
            false, db
