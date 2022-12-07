module CustomCommand.Model
open FsharpMyExtension
open FsharpMyExtension.ResultExt
open MongoDB.Driver
open MongoDB.Bson

open Types
open Db

type Embed =
    {
        Description: MessageTemplate.MessageRaw option
        ImageUrl: MessageTemplate.MessageRaw option
    }

type Message =
    {
        Content: MessageTemplate.MessageRaw option
        Embed: Embed
    }

type EffectV0 =
    {
        OnSelf: Message
        OnBot: Message
        OnOther: Message
    }

type CommandId = System.Guid
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module CommandId =
    let create () =
        System.Guid.NewGuid()

    let serialize (id: CommandId) =
        id.ToString()

    let tryDeserialize (str: string) =
        match System.Guid.TryParse str with
        | true, x -> Ok x
        | false, _ -> Error ""

type CommandDataV0 =
    {
        Names: string []
        RandomEffects: EffectV0 []
    }
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module CommandDataV0 =
    let empty =
        {
            Names = [||]
            RandomEffects = [||]
        }

type CommandData =
    {
        Names: string []
        OnSelf: Message []
        OnBot: Message []
        OnOther: Message []
    }
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module CommandData =
    let empty: CommandData =
        {
            Names = [||]
            OnSelf = [||]
            OnBot = [||]
            OnOther = [||]
        }

    let create names onSelf onBot onOther: CommandData =
        {
            Names = names
            OnSelf = onSelf
            OnBot = onBot
            OnOther = onOther
        }

    let serialize (data: CommandDataV0) =
        data |> Json.ser

    let deserialize json =
        try
            let data: CommandDataV0 = Json.des json
            Ok data
        with e ->
            Error e.Message

type Version =
    | V0 = 0
    | V1 = 1

type Command<'Data> =
    {
        Id: CommandId
        Version: Version
        Data: 'Data
    }
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Command =
    let create id (data: 'ItemData) =
        {
            Id = id
            Version = Version.V0
            Data = data
        }

/// Current version of item
type CommandT = CommonDb.Data<CommandId, Version, CommandData>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module CommandT =
    let create id data: CommandT =
        CommonDb.Data.create id Version.V1 data

type CommandsArray = CommandT []
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module CommandsArray =
    let serialize (item: CommandsArray) =
        Json.ser item

    let tryDeserialize json =
        try
            let res: CommandsArray = Json.des json
            Ok res
        with e ->
            Error e.Message

type Commands = CommonDb.GuildData<CommandId, Version, CommandData>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Commands =
    let createData id =
        CommandT.create id CommandData.empty

    let init collectionName (db: IMongoDatabase): Commands =
        CommonDb.GuildData.init
            createData
            (fun ver doc ->
                match ver with
                | Some ver ->
                    match ver with
                    | Version.V1 ->
                        None, Serialization.BsonSerializer.Deserialize<CommandT>(doc)
                    | Version.V0 ->
                        let oldItem = Serialization.BsonSerializer.Deserialize<CommonDb.Data<CommandId, Version, CommandDataV0>>(doc)

                        let randomEffects =
                            oldItem.Data.RandomEffects

                        let onSelfs =
                            randomEffects
                            |> Array.map (fun x ->
                                x.OnSelf
                            )
                        let onBots =
                            randomEffects
                            |> Array.map (fun x ->
                                x.OnBot
                            )
                        let onOthers =
                            randomEffects
                            |> Array.map (fun x ->
                                x.OnOther
                            )

                        let newItem: CommandT =
                            CommandT.create
                                oldItem.Id
                                (CommandData.create oldItem.Data.Names onSelfs onBots onOthers)

                        Some oldItem.Id, newItem
                    | x ->
                        failwithf "Version = %A not implemented" x
                | None ->
                    failwith "Expected V0 but DataPreVersion"
            )
            collectionName
            db

    let set (itemIdOpt: option<CommandId>) setAdditionParams (items: Commands) =
        let id =
            itemIdOpt
            |> Option.defaultWith
                (fun () ->
                    CommandId.create()
                )

        CommonDb.GuildData.set
            createData
            id
            setAdditionParams
            items

    let sets (items: CommandsArray) (db: Commands): Commands =
        db
        |> CommonDb.GuildData.sets items

    let drop (db: IMongoDatabase) (items: Commands) =
        CommonDb.GuildData.drop db items

    let tryFindById id (items: Commands): CommandT option =
        Map.tryFind id items.Cache
