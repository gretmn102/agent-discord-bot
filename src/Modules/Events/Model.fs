module Events.Model
open FsharpMyExtension
open MongoDB.Driver
open MongoDB.Bson
open MongoDB.Bson.Serialization.Attributes
open DiscordBotExtensions.Types
open DiscordBotExtensions.Db

[<BsonIgnoreExtraElements>]
type DataPreVersion =
    {
        Id: ObjectId
        GuildId: GuildId
        FilteringRoleId: RoleId
        IsEnabled: bool
    }

type GuildDataV0 =
    {
        FilteringRoleId: RoleId
        IsEnabled: bool
    }

type GuildData =
    {
        FilteringRoleId: RoleId Set
        IsEnabled: bool
        Emojis: string []
    }
    static member Init filteringRoleId isEnabled emojis : GuildData =
        {
            FilteringRoleId = filteringRoleId
            IsEnabled = isEnabled
            Emojis = emojis
        }
    static member Empty =
        {
            FilteringRoleId = Set.empty
            IsEnabled = false
            Emojis = [||]
        }
    static member Serialize (data: GuildData) =
        data |> Json.ser
    static member Deserialize json =
        try
            let x: GuildData = Json.des json
            Ok x
        with e ->
            Error e.Message

type Version =
    | V0 = 0
    | V1 = 1

type Id = UserId

type Guild = CommonDb.Data<Id, Version, GuildData>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Guild =
    let create id data: Guild =
        CommonDb.Data.create id Version.V1 data

type Guilds = CommonDb.GuildData<Id, Version, GuildData>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Guilds =
    let createData id =
        Guild.create id GuildData.Empty

    let init collectionName (db: IMongoDatabase): Guilds =
        CommonDb.GuildData.init
            createData
            (fun ver doc ->
                match ver with
                | Some ver ->
                    match ver with
                    | Version.V1 ->
                        None, Serialization.BsonSerializer.Deserialize<Guild>(doc)
                    | Version.V0 ->
                        let oldItem =
                            Serialization.BsonSerializer.Deserialize<CommonDb.Data<Id, Version, GuildDataV0>>(doc)
                        let newItem =
                            Guild.create
                                oldItem.Id
                                (GuildData.Init
                                    (Set.singleton oldItem.Data.FilteringRoleId)
                                    oldItem.Data.IsEnabled
                                    [||])

                        Some (box oldItem.Id), newItem
                    | x ->
                        failwithf "Version = %A not implemented" x
                | None ->
                    let oldValue =
                        Serialization.BsonSerializer.Deserialize<DataPreVersion>(doc)
                    let newValue =
                        GuildData.Init (Set.singleton oldValue.FilteringRoleId) oldValue.IsEnabled [||]
                        |> Guild.create oldValue.GuildId

                    Some (box oldValue.Id), newValue
            )
            collectionName
            db

    let set userId setAdditionParams (guildData: Guilds) =
        CommonDb.GuildData.set
            createData
            userId
            setAdditionParams
            guildData

    let drop (db: IMongoDatabase) (items: Guilds) =
        CommonDb.GuildData.drop db items

    let tryFindById id (items: Guilds): Guild option =
        CommonDb.GuildData.tryFind id items
