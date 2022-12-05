module Events.Model
open FsharpMyExtension
open MongoDB.Driver
open MongoDB.Bson
open MongoDB.Bson.Serialization.Attributes

open Types
open Db

[<BsonIgnoreExtraElements>]
type DataPreVersion =
    {
        Id: ObjectId
        GuildId: GuildId
        FilteringRoleId: RoleId
        IsEnabled: bool
    }

type GuildData =
    {
        FilteringRoleId: RoleId
        IsEnabled: bool
    }
    static member Init filteringRoleId isEnabled =
        {
            FilteringRoleId = filteringRoleId
            IsEnabled = isEnabled
        }
    static member Empty =
        {
            FilteringRoleId = 0UL
            IsEnabled = false
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

type Guild = CommonDb.Data<Id, Version, GuildData>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Guild =
    let create id data: Guild =
        CommonDb.Data.create id Version.V0 data

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
                    | Version.V0 ->
                        None, Serialization.BsonSerializer.Deserialize<Guild>(doc)
                    | x ->
                        failwithf "Version = %A not implemented" x
                | None ->
                    let oldValue =
                        Serialization.BsonSerializer.Deserialize<DataPreVersion>(doc)
                    let newValue =
                        GuildData.Init oldValue.FilteringRoleId oldValue.IsEnabled
                        |> Guild.create oldValue.GuildId

                    Some oldValue.Id, newValue
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
