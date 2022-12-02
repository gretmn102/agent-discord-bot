module EggBattle.Model
open FsharpMyExtension
open MongoDB.Driver
open MongoDB.Bson
open MongoDB.Bson.Serialization.Attributes

open Types
open Db

module Rating =
    [<BsonIgnoreExtraElements>]
    type DataPreVersion =
        {
            Id: ObjectId
            GuildId: GuildId
            UserId: UserId
            Wins: int
            Loses: int
        }

    type GuildUserData =
        {
            Wins: int
            Loses: int
        }
        static member Init wins loses =
            {
                Wins = wins
                Loses = loses
            }
        static member Empty =
            {
                Wins = 0
                Loses = 0
            }
        static member Serialize (data: GuildUserData) =
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
            UserId: UserId
        }
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Id =
        let create guildId userId =
            {
                GuildId = guildId
                UserId = userId
            }

    type GuildUser = CommonDb.Data<Id, Version, GuildUserData>
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module GuildUser =
        let create id data: GuildUser =
            CommonDb.Data.create id Version.V0 data

    type GuildUsers = CommonDb.GuildData<Id, Version, GuildUserData>
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module GuildUsers =
        let createData id =
            GuildUser.create id GuildUserData.Empty

        let init collectionName (db: IMongoDatabase): GuildUsers =
            CommonDb.GuildData.init
                createData
                (fun ver doc ->
                    match ver with
                    | Some ver ->
                        match ver with
                        | Version.V0 ->
                            None, Serialization.BsonSerializer.Deserialize<GuildUser>(doc)
                        | x ->
                            failwithf "Version = %A not implemented" x
                    | None ->
                        let oldValue =
                            Serialization.BsonSerializer.Deserialize<DataPreVersion>(doc)
                        let newValue =
                            GuildUserData.Init oldValue.Wins oldValue.Loses
                            |> GuildUser.create (Id.create oldValue.GuildId oldValue.UserId)

                        Some oldValue.Id, newValue
                )
                collectionName
                db

        let set userId setAdditionParams (guildData: GuildUsers) =
            CommonDb.GuildData.set
                createData
                userId
                setAdditionParams
                guildData

        let drop (db: IMongoDatabase) (items: GuildUsers) =
            CommonDb.GuildData.drop db items

        let tryFindById id (items: GuildUsers) =
            CommonDb.GuildData.tryFind id items
