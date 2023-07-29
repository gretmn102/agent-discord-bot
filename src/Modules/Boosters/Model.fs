module Boosters.Model
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
        OutputChannelId: ChannelId
        Message: string
    }

type GuildData =
    {
        OutputChannelId: ChannelId
        Message: string
    }
    static member Init outputChannelId message =
        {
            OutputChannelId = outputChannelId
            Message = message
        }
    static member Empty =
        {
            OutputChannelId = 0UL
            Message = ""
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

type Id = UserId

type GuildSetting = CommonDb.Data<Id, Version, GuildData>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module GuildSetting =
    let create id data: GuildSetting =
        CommonDb.Data.create id Version.V0 data

type GuildSettings = CommonDb.GuildData<Id, Version, GuildData>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module GuildSettings =
    let createData id =
        GuildSetting.create id GuildData.Empty

    let init collectionName (db: IMongoDatabase): GuildSettings =
        CommonDb.GuildData.init
            createData
            (fun ver doc ->
                match ver with
                | Some ver ->
                    match ver with
                    | Version.V0 ->
                        None, Serialization.BsonSerializer.Deserialize<GuildSetting>(doc)
                    | x ->
                        failwithf "Version = %A not implemented" x
                | None ->
                    let oldValue =
                        Serialization.BsonSerializer.Deserialize<DataPreVersion>(doc)
                    let newValue =
                        GuildData.Init oldValue.OutputChannelId oldValue.Message
                        |> GuildSetting.create oldValue.GuildId

                    Some oldValue.Id, newValue
            )
            collectionName
            db

    let set userId setAdditionParams (guildData: GuildSettings) =
        CommonDb.GuildData.set
            createData
            userId
            setAdditionParams
            guildData

    let drop (db: IMongoDatabase) (items: GuildSettings) =
        CommonDb.GuildData.drop db items

    let tryFindById id (items: GuildSettings) =
        CommonDb.GuildData.tryFind id items
