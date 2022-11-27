module Age.Model
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
        UserId: UserId
        Age: int
        GuildIds: GuildId Set
    }

type UserData =
    {
        Age: int
        GuildIds: GuildId Set
    }
    static member Init age guildIds =
        {
            Age = age
            GuildIds = guildIds
        }
    static member Empty =
        {
            Age = 0
            GuildIds = Set.empty
        }
    static member Serialize (data: UserData) =
        data |> Json.ser
    static member Deserialize json =
        try
            Ok (Json.des json)
        with e ->
            Error e.Message

type Version =
    | V0 = 0

type Id = UserId

type User = CommonDb.Data<Id, Version, UserData>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module User =
    let create id data: User =
        CommonDb.Data.create id Version.V0 data

type Users = CommonDb.GuildData<Id, Version, UserData>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Users =
    let createData id =
        User.create id UserData.Empty

    let init collectionName (db: IMongoDatabase): Users =
        CommonDb.GuildData.init
            createData
            (fun ver doc ->
                match ver with
                | Some ver ->
                    match ver with
                    | Version.V0 ->
                        None, Serialization.BsonSerializer.Deserialize<User>(doc)
                    | x ->
                        failwithf "Version = %A not implemented" x
                | None ->
                    let oldValue =
                        Serialization.BsonSerializer.Deserialize<DataPreVersion>(doc)
                    let newValue =
                        UserData.Init oldValue.Age oldValue.GuildIds
                        |> User.create oldValue.UserId

                    Some oldValue.Id, newValue
            )
            collectionName
            db

    let set userId setAdditionParams (guildData: Users) =
        CommonDb.GuildData.set
            createData
            userId
            setAdditionParams
            guildData

    let drop (db: IMongoDatabase) (items: Users) =
        CommonDb.GuildData.drop db items

    let tryFindById id (items: Users) =
        CommonDb.GuildData.tryFind id items
