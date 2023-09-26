module CustomCommand.Model
open FsharpMyExtension
open FsharpMyExtension.ResultExt
open MongoDB.Driver
open MongoDB.Bson
open DiscordBotExtensions.Types
open DiscordBotExtensions.Db

type Embed =
    {
        Description: MessageTemplate.MessageRaw option
        ImageUrl: MessageTemplate.MessageRaw option
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Embed =
    let empty : Embed =
        {
            Description = None
            ImageUrl = None
        }

    let create description imageUrl : Embed =
        {
            Description = description
            ImageUrl = imageUrl
        }

type Message =
    {
        Content: MessageTemplate.MessageRaw option
        Embed: Embed
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Message =
    let empty : Message =
        {
            Content = None
            Embed = Embed.empty
        }

    let create content embed : Message =
        {
            Content = content
            Embed = embed
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

type CommandDataV1 =
    {
        Names: string []
        OnSelf: Message []
        OnBot: Message []
        OnOther: Message []
    }
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module CommandDataV1 =
    let create names onSelf onBot onOther: CommandDataV1 =
        {
            Names = names
            OnSelf = onSelf
            OnBot = onBot
            OnOther = onOther
        }

type Reaction =
    {
        /// `P = m / n`, where `P` — probability of current event, `m` — favorable results, `n` — sum of all favorable results in reactions.
        ProbabilityFavorableResults: int
        Message: Message
    }
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Reaction =
    let create probabilityFavorableResults messages : Reaction =
        {
            ProbabilityFavorableResults = probabilityFavorableResults
            Message = messages
        }

type ReactionsList = Reaction []

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module ReactionsList =
    let r = System.Random ()

    let randomGetWithCustomRandom genRandomMinMax (reactions: ReactionsList) : Reaction =
        let xs, possibleOutcomes =
            reactions
            |> Array.mapFold
                (fun sum reaction ->
                    sum, sum + reaction.ProbabilityFavorableResults
                )
                0

        let result = genRandomMinMax 0 possibleOutcomes
        xs
        |> Array.binarySearch
            (fun x ->
                x.CompareTo result
            )
        |> fun index ->
            reactions.[index]

    let randomGet (reactions: ReactionsList) : Reaction =
        reactions
        |> randomGetWithCustomRandom (fun min max -> r.Next(min, max))

type CommandData =
    {
        Names: string []
        OnSelf: ReactionsList
        OnBot: ReactionsList
        OnOther: ReactionsList
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
    | V2 = 2

type CommandV1 = CommonDb.Data<CommandId, Version, CommandDataV1>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module CommandV1 =
    let create id data: CommandV1 =
        CommonDb.Data.create id Version.V1 data

/// Current version of item
type Command = CommonDb.Data<CommandId, Version, CommandData>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Command =
    let create id data: Command =
        CommonDb.Data.create id Version.V2 data

type CommandsArray = Command []
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
        Command.create id CommandData.empty

    let rec migrate ver (doc: BsonDocument) =
        match ver with
        | Version.V2 ->
            None, Serialization.BsonSerializer.Deserialize<Command>(doc)

        | Version.V1 ->
            let oldItem = Serialization.BsonSerializer.Deserialize<CommonDb.Data<CommandId, Version, CommandDataV1>>(doc)
            let equiprobable (messages: Message []) =
                messages
                |> Array.map (fun x ->
                    Reaction.create 1 x
                )

            let newItem =
                Command.create
                    oldItem.Id
                    (CommandData.create
                        oldItem.Data.Names
                        (equiprobable oldItem.Data.OnSelf)
                        (equiprobable oldItem.Data.OnBot)
                        (equiprobable oldItem.Data.OnOther))

            Some oldItem.Id, newItem

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

            let newItem =
                CommandV1.create
                    oldItem.Id
                    (CommandDataV1.create oldItem.Data.Names onSelfs onBots onOthers)

            migrate newItem.Version (newItem.ToBsonDocument())
        | x ->
            failwithf "Version = %A not implemented" x

    let init collectionName (db: IMongoDatabase): Commands =
        CommonDb.GuildData.init
            createData
            (fun ver doc ->
                match ver with
                | Some ver ->
                    migrate ver doc
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

    let tryFindById id (items: Commands): Command option =
        Map.tryFind id items.Cache
