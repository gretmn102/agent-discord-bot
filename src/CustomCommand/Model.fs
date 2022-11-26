module CustomCommand.Model
open FsharpMyExtension
open FsharpMyExtension.ResultExt
open MongoDB.Driver
open MongoDB.Bson

open Types

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

type Effect =
    {
        OnSelf: Message
        OnBot: Message
        OnOther: Message
    }

type CommandId = System.Guid
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module CommandId =
    let serialize (id: CommandId) =
        id.ToString()

    let tryDeserialize (str: string) =
        match System.Guid.TryParse str with
        | true, x -> Ok x
        | false, _ -> Error ""

type CommandData =
    {
        Names: string []
        RandomEffects: Effect []
    }
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module CommandData =
    let empty =
        {
            Names = [||]
            RandomEffects = [||]
        }

    let create names randomEffects =
        {
            Names = names
            RandomEffects = randomEffects
        }

    let serialize (data: CommandData) =
        data |> Json.ser

    let deserialize json =
        try
            let data: CommandData = Json.des json
            Ok data
        with e ->
            Error e.Message

type Version =
    | V0 = 0

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
type CommandT = Command<CommandData>

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

type Collection = IMongoCollection<BsonDocument>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Collection =
    let replace (newData: CommandT) (collection: Collection) =
        let doc = newData.ToBsonDocument()

        let el = BsonElement("_id", BsonValue.Create(newData.Id))
        let i = new BsonDocument(el)

        let filter = FilterDefinition.op_Implicit(i)

        collection.ReplaceOne(filter, doc)
        |> ignore

    let insert id setAdditionParams (collection: Collection): Command<'ItemData> =
        let id =
            id
            |> Option.defaultWith (fun () ->
                System.Guid.NewGuid()
            )

        let x = Command.create id (setAdditionParams CommandData.empty)
        collection.InsertOne(x.ToBsonDocument())
        x

type Commands =
    {
        Cache: Map<CommandId, CommandT>
        Collection: Collection
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Commands =
    let collectionName = "customCommands"

    let init (db: IMongoDatabase): Commands =
        let collection = db.GetCollection<BsonDocument>(collectionName)

        if IMongoCollection.isEmpty collection then
            let item =
                {
                    Cache = Map.empty
                    Collection = collection
                }

            item
        else
            {
                Cache =
                    collection.Find(fun x -> true).ToEnumerable()
                    |> Seq.fold
                        (fun st x ->
                            let ver =
                                match x.["Version"] with
                                | null -> failwithf "`Version` but\n%A" x
                                | x ->
                                    if x.IsInt32 then
                                        enum<Version> x.AsInt32
                                    else
                                        failwithf "Version not int32 but %A" x
                            let x =
                                match ver with
                                | Version.V0 ->
                                    Serialization.BsonSerializer.Deserialize<CommandT>(x)
                                | x ->
                                    failwithf "Version = %A not implemented" x

                            Map.add x.Id x st
                        )
                        Map.empty
                Collection = collection
            }

    let set itemIdOpt setAdditionParams (items: Commands) =
        let cache = items.Cache

        {
            items with
                Cache =
                    let insertNew id =
                        let item =
                            items.Collection
                            |> Collection.insert id setAdditionParams

                        Map.add item.Id item cache

                    match itemIdOpt with
                    | Some itemId ->
                        match Map.tryFind itemId cache with
                        | Some item ->
                            let item =
                                { item with
                                    Data = setAdditionParams item.Data
                                }

                            Collection.replace item items.Collection

                            Map.add itemId item cache
                        | None ->
                            insertNew (Some itemId)
                    | None ->
                        insertNew None
        }

    let sets (items: CommandsArray) (db: Commands): Commands =
        items
        |> Array.fold
            (fun items item ->
                let rec f () =
                    try
                        set (Some item.Id) (fun _ -> item.Data) items
                    with e ->
                        printfn "sets error:\n%s" e.Message
                        System.Threading.Thread.Sleep 500
                        f ()
                f ()
            )
            db

    let drop (db: IMongoDatabase) (items: Commands) =
        db.DropCollection collectionName

        { items with
            Cache = Map.empty
        }

    let tryFindById id (items: Commands) =
        Map.tryFind id items.Cache
