module Doorkeeper.Model
open FsharpMyExtension
open MongoDB.Driver
open MongoDB.Bson

open Types
open Shared.MessageTemplate

module NewcomersRolesOld =
    type PassSettings =
        {
            PermittedRoles: RoleId Set
            MainChannelId: ChannelId
            WelcomeMessage: string
            /// dictionary key is the role name that is specified in the pass command
            IssuedRoleIds: (string * RoleId) []
            PassLogMessage: (ChannelId * string) option
        }
        static member Empty = {
            PermittedRoles = Set.empty
            MainChannelId = 0UL
            WelcomeMessage = ""
            IssuedRoleIds = [||]
            PassLogMessage = None
        }

        static member SampleJson =
            {
                PermittedRoles = Set [ 12345678UL ]
                MainChannelId = 124576UL
                WelcomeMessage = "<@userName>, welcome to the club!"
                IssuedRoleIds = [|
                    "first", 12345678UL
                    "second", 123456789UL
                |]
                PassLogMessage = Some (12345678UL, "<@userMention> is let in")
            }
            |> Json.ser

    type NewcomersRolesData =
        {
            mutable Id: ObjectId
            mutable GuildId: GuildId
            mutable RoleIds: RoleId Set

            mutable PassSettings: PassSettings option
        }
        static member Init(guildId: GuildId) =
            {
                Id = ObjectId.Empty
                GuildId = guildId
                RoleIds = Set.empty

                PassSettings = None
            }

    type Collection = IMongoCollection<NewcomersRolesData>

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Collection =
        let replace (newData: NewcomersRolesData) (welcomeSetting: Collection) =
            welcomeSetting.ReplaceOne((fun x -> x.Id = newData.Id), newData)
            |> ignore

        let insert guildId setAdditionParams (welcomeSetting: Collection) =
            let x = setAdditionParams (NewcomersRolesData.Init guildId)
            welcomeSetting.InsertOne(x)
            x

    type GuildNewcomersRoles =
        {
            Cache: Map<GuildId, NewcomersRolesData>
            Collection: Collection
        }

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module GuildNewcomersRoles =
        let collectionName = "newcomersRoles"

        let getAll (db: IMongoDatabase): GuildNewcomersRoles =
            let welcomeSetting = db.GetCollection<NewcomersRolesData>(collectionName)

            {
                Cache =
                    welcomeSetting.Find(fun x -> true).ToEnumerable()
                    |> Seq.fold
                        (fun st x ->
                            Map.add x.GuildId x st
                        )
                        Map.empty
                Collection = welcomeSetting
            }

        let setWelcomeSetting guildId setAdditionParams (guildWelcomeSetting: GuildNewcomersRoles) =
            let cache = guildWelcomeSetting.Cache

            {
                guildWelcomeSetting with
                    Cache =
                        match Map.tryFind guildId cache with
                        | Some welcomeSettingData ->
                            let newcomersRoles =
                                setAdditionParams welcomeSettingData

                            Collection.replace newcomersRoles guildWelcomeSetting.Collection

                            Map.add guildId newcomersRoles cache
                        | None ->
                            let x =
                                guildWelcomeSetting.Collection
                                |> Collection.insert guildId setAdditionParams

                            Map.add guildId x cache
            }

        let tryFind guildId (guildWelcomeSetting: GuildNewcomersRoles) =
            Map.tryFind guildId guildWelcomeSetting.Cache

        let drop (db: IMongoDatabase) (guildWelcomeSetting: GuildNewcomersRoles) =
            db.DropCollection collectionName

            { guildWelcomeSetting with
                Cache = Map.empty
            }

module WelcomeSettingOld =
    type ChannelMessage =
        {
            ChannelId: ChannelId
            Message: string
        }
        static member Init channelId message =
            { ChannelId = channelId; Message = message }

    type WelcomeSettingData =
        {
            mutable Id: ObjectId
            mutable GuildId: GuildId
            mutable OutputChannel: ChannelId option
            mutable TemplateMessage: string option
            mutable OutputLogChannel: ChannelId option
            mutable TemplateLogMessage: string option
            mutable OutputLeaveChannel: ChannelId option
            mutable TemplateLeaveMessage: string option
            mutable LeaversChannelMessage: ChannelMessage option
            mutable LeaversLogChannelMessage: ChannelMessage option
        }
        static member Init guildId =
            {
                Id = ObjectId.Empty
                GuildId = guildId
                OutputChannel = None
                TemplateMessage = None
                OutputLogChannel = None
                TemplateLogMessage = None
                OutputLeaveChannel = None
                TemplateLeaveMessage = None
                LeaversChannelMessage = None
                LeaversLogChannelMessage = None
            }


    type Collection = IMongoCollection<WelcomeSettingData>

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Collection =
        let replace (newData: WelcomeSettingData) (welcomeSetting: Collection) =
            welcomeSetting.ReplaceOne((fun x -> x.Id = newData.Id), newData)
            |> ignore

        let insert guildId setAdditionParams (welcomeSetting: Collection) =
            let x = setAdditionParams (WelcomeSettingData.Init guildId)
            welcomeSetting.InsertOne(x)
            x

    type GuildWelcomeSetting =
        {
            Cache: Map<GuildId, WelcomeSettingData>
            Collection: Collection
        }

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module GuildWelcomeSetting =
        let collectionName = "welcomeSetting"

        let getAll (db: IMongoDatabase): GuildWelcomeSetting =
            let welcomeSetting = db.GetCollection<WelcomeSettingData>(collectionName)

            {
                Cache =
                    welcomeSetting.Find(fun x -> true).ToEnumerable()
                    |> Seq.fold
                        (fun st x ->
                            Map.add x.GuildId x st
                        )
                        Map.empty
                Collection = welcomeSetting
            }

        let setWelcomeSetting guildId setAdditionParams (guildWelcomeSetting: GuildWelcomeSetting) =
            let cache = guildWelcomeSetting.Cache

            {
                guildWelcomeSetting with
                    Cache =
                        match Map.tryFind guildId cache with
                        | Some welcomeSettingData ->
                            let newcomersRoles =
                                setAdditionParams welcomeSettingData

                            Collection.replace newcomersRoles guildWelcomeSetting.Collection

                            Map.add guildId newcomersRoles cache
                        | None ->
                            let x =
                                guildWelcomeSetting.Collection
                                |> Collection.insert guildId setAdditionParams

                            Map.add guildId x cache
            }

        let tryFind guildId (guildWelcomeSetting: GuildWelcomeSetting) =
            Map.tryFind guildId guildWelcomeSetting.Cache

        let drop (db: IMongoDatabase) (guildWelcomeSetting: GuildWelcomeSetting) =
            db.DropCollection collectionName

            { guildWelcomeSetting with
                Cache = Map.empty
            }

module Setting =
    type Checkpoint =
        {
            Channel: EnabledOptionValue<ChannelId>
            DoorkeeperRole: EnabledOptionValue<RoleId>
            /// dictionary key is the role name that is specified in the pass command
            IssuedRoleIds: EnabledOptionValue<(string * RoleId) []>
            EnteredUserRole: EnabledOptionValue<Set<RoleId>>
            NewcomerWelcomeMessage: EnabledOptionValue<MessageRaw>
            NewcomerWelcomeMessageLog: EnabledOptionValue<MessageRaw>
            ReturnedWelcomeMessage: EnabledOptionValue<MessageRaw>
            ReturnedWelcomeMessageLog: EnabledOptionValue<MessageRaw>
        }
        static member Empty =
            {
                Channel = EnabledOptionValue.Empty
                DoorkeeperRole = EnabledOptionValue.Empty
                IssuedRoleIds = EnabledOptionValue.Empty
                EnteredUserRole = EnabledOptionValue.Empty
                NewcomerWelcomeMessage = EnabledOptionValue.Empty
                NewcomerWelcomeMessageLog = EnabledOptionValue.Empty
                ReturnedWelcomeMessage = EnabledOptionValue.Empty
                ReturnedWelcomeMessageLog = EnabledOptionValue.Empty
            }
        static member Sample =
            {
                Channel =
                    12345678UL
                    |> EnabledOptionValue.Init
                DoorkeeperRole =
                    12345678UL
                    |> EnabledOptionValue.Init
                IssuedRoleIds =
                    [|
                        "warrior", 123456789UL
                        "archer", 123456789UL
                        "wizzard", 123456789UL
                    |]
                    |> EnabledOptionValue.Init
                EnteredUserRole =
                    [|
                        123456789UL
                    |]
                    |> Set.ofArray
                    |> EnabledOptionValue.Init
                NewcomerWelcomeMessage =
                    "<@userMention>, welcome to our guild!"
                    |> EnabledOptionValue.Init
                NewcomerWelcomeMessageLog =
                    "<@userMention> entered to guild!"
                    |> EnabledOptionValue.Init
                ReturnedWelcomeMessage =
                    "<@userMention>, welcome back!"
                    |> EnabledOptionValue.Init
                ReturnedWelcomeMessageLog =
                    "<@userMention> returned to us"
                    |> EnabledOptionValue.Init
            }

    type Inner =
        {
            Channel: EnabledOptionValue<ChannelId>
            NewcomerWelcomeMessage: EnabledOptionValue<MessageRaw>
            NewcomerWelcomeMessageLog: EnabledOptionValue<MessageRaw>
        }
        static member Empty =
            {
                Channel = EnabledOptionValue.Empty
                NewcomerWelcomeMessage = EnabledOptionValue.Empty
                NewcomerWelcomeMessageLog = EnabledOptionValue.Empty
            }
        static member Sample =
            {
                Channel =
                    124576UL
                    |> EnabledOptionValue.Init
                NewcomerWelcomeMessage =
                    "<@userMention>, welcome to the club!"
                    |> EnabledOptionValue.Init
                NewcomerWelcomeMessageLog =
                    "<@userName> is let in"
                    |> EnabledOptionValue.Init
            }


    type Exit =
        {
            Channel: EnabledOptionValue<ChannelId>
            GoodbyeMessage: EnabledOptionValue<MessageRaw>
            GoodbyeMessageLog: EnabledOptionValue<MessageRaw>
        }
        static member Empty =
            {
                Channel = EnabledOptionValue.Empty
                GoodbyeMessage = EnabledOptionValue.Empty
                GoodbyeMessageLog = EnabledOptionValue.Empty
            }
        static member Sample =
            {
                Channel =
                    124576UL
                    |> EnabledOptionValue.Init
                GoodbyeMessage =
                    "<@userMention>, welcome to the club!"
                    |> EnabledOptionValue.Init
                GoodbyeMessageLog =
                    "<@userName> is let in"
                    |> EnabledOptionValue.Init
            }

    type Log =
        {
            Channel: EnabledOptionValue<ChannelId>
        }
        static member Empty =
            {
                Channel = EnabledOptionValue.Empty
            }
        static member Sample =
            {
                Channel =
                    124576UL
                    |> EnabledOptionValue.Init
            }

    type MainData =
        {
            Checkpoint: Checkpoint
            Inner: Inner
            Exit: Exit
            Log: Log
        }
        static member Empty =
            {
                Checkpoint = Checkpoint.Empty
                Inner = Inner.Empty
                Exit = Exit.Empty
                Log = Log.Empty
            }
        static member Sample =
            {
                Checkpoint = Checkpoint.Sample
                Inner = Inner.Sample
                Exit = Exit.Sample
                Log = Log.Sample
            }
        static member Serialize (data: MainData) =
            data |> Json.ser
        static member Deserialize json =
            try
                Ok (Json.des json)
            with e ->
                Error e.Message

    type Version =
        | V0 = 0

    type Data<'MainData> =
        {
            Id: ObjectId
            GuildId: GuildId
            Version: Version
            Data: 'MainData
        }
        static member Init(data: 'MainData, guildId: GuildId) =
            {
                Id = ObjectId.Empty
                GuildId = guildId
                Version = Version.V0
                Data = data
            }

    type Collection = IMongoCollection<BsonDocument>

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Collection =
        let replace (newData: Data<MainData>) (collection: Collection) =
            let doc = newData.ToBsonDocument()

            let el = BsonElement("_id", BsonValue.Create(newData.Id))
            let i = new BsonDocument(el)

            let filter = FilterDefinition.op_Implicit(i)

            collection.ReplaceOne(filter, doc)
            |> ignore

        let insert guildId setAdditionParams (collection: Collection) =
            let x =
                let data = Data.Init(MainData.Empty, guildId)
                { data with
                    Data = setAdditionParams data.Data
                }

            let d = x.ToBsonDocument()
            collection.InsertOne(d)

            let newId = d.["_id"].AsObjectId

            { x with
                Id = newId
            }

    type GuildData =
        {
            Cache: Map<GuildId, Data<MainData>>
            Collection: Collection
        }

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module GuildData =
        let set guildId setAdditionParams (guildData: GuildData) =
            let cache = guildData.Cache

            {
                guildData with
                    Cache =
                        match Map.tryFind guildId cache with
                        | Some fullData ->
                            let data =
                                { fullData with
                                    Data = setAdditionParams fullData.Data
                                }

                            Collection.replace data guildData.Collection

                            Map.add guildId data cache
                        | None ->
                            let x =
                                guildData.Collection
                                |> Collection.insert guildId setAdditionParams

                            Map.add guildId x cache
            }

        let collectionName = "doorkeeperSetting"

        let init (db: IMongoDatabase): GuildData =
            let collection = db.GetCollection<BsonDocument>(collectionName)

            if IMongoCollection.isEmpty collection then
                let guildData =
                    {
                        Cache = Map.empty
                        Collection = collection
                    }

                let permissiveRoles = NewcomersRolesOld.GuildNewcomersRoles.getAll db
                let guildData =
                    permissiveRoles.Collection.Find(fun _ -> true).ToEnumerable()
                    |> Seq.fold
                        (fun st x ->
                            st
                            |> set
                                x.GuildId
                                (fun data ->
                                    { data with
                                        Checkpoint =
                                            { data.Checkpoint with
                                                DoorkeeperRole =
                                                    x.PassSettings
                                                    |> Option.bind (fun x ->
                                                        x.PermittedRoles
                                                        |> Seq.tryHead
                                                    )
                                                    |> Option.map EnabledOptionValue.Init
                                                    |> Option.defaultValue EnabledOptionValue.Empty
                                                EnteredUserRole =
                                                    x.RoleIds
                                                    |> EnabledOptionValue.Init
                                                IssuedRoleIds =
                                                    x.PassSettings
                                                    |> Option.map (fun x -> x.IssuedRoleIds)
                                                    |> Option.map EnabledOptionValue.Init
                                                    |> Option.defaultValue EnabledOptionValue.Empty
                                            }
                                        Inner =
                                            { data.Inner with
                                                Channel =
                                                    x.PassSettings
                                                    |> Option.map (fun x -> x.MainChannelId)
                                                    |> Option.map EnabledOptionValue.Init
                                                    |> Option.defaultValue EnabledOptionValue.Empty
                                                NewcomerWelcomeMessage =
                                                    x.PassSettings
                                                    |> Option.map (fun x -> x.WelcomeMessage)
                                                    |> Option.map EnabledOptionValue.Init
                                                    |> Option.defaultValue EnabledOptionValue.Empty
                                                NewcomerWelcomeMessageLog =
                                                    x.PassSettings
                                                    |> Option.bind (fun x ->
                                                        x.PassLogMessage
                                                        |> Option.map (fun (channelId, message) ->
                                                            message
                                                        )
                                                    )
                                                    |> Option.map EnabledOptionValue.Init
                                                    |> Option.defaultValue EnabledOptionValue.Empty
                                            }

                                        Log =
                                            { data.Log with
                                                Channel =
                                                    x.PassSettings
                                                    |> Option.bind (fun x ->
                                                        x.PassLogMessage
                                                        |> Option.map (fun (channelId, message) ->
                                                            channelId
                                                        )
                                                    )
                                                    |> Option.map EnabledOptionValue.Init
                                                    |> Option.defaultValue EnabledOptionValue.Empty
                                            }

                                    }
                                )
                        )
                        guildData

                let templateRoles = WelcomeSettingOld.GuildWelcomeSetting.getAll db
                let guildData =
                    templateRoles.Collection.Find(fun _ -> true).ToEnumerable()
                    |> Seq.fold
                        (fun st x ->
                            st
                            |> set
                                x.GuildId
                                (fun data ->
                                    { data with
                                        Checkpoint =
                                            { data.Checkpoint with
                                                Channel =
                                                    x.OutputChannel
                                                    |> Option.map EnabledOptionValue.Init
                                                    |> Option.defaultValue EnabledOptionValue.Empty
                                                NewcomerWelcomeMessage =
                                                    x.TemplateMessage
                                                    |> Option.map EnabledOptionValue.Init
                                                    |> Option.defaultValue EnabledOptionValue.Empty
                                                NewcomerWelcomeMessageLog =
                                                    x.TemplateLogMessage
                                                    |> Option.map EnabledOptionValue.Init
                                                    |> Option.defaultValue EnabledOptionValue.Empty
                                                ReturnedWelcomeMessage =
                                                    x.LeaversChannelMessage
                                                    |> Option.map (fun x -> x.Message)
                                                    |> Option.map EnabledOptionValue.Init
                                                    |> Option.defaultValue EnabledOptionValue.Empty
                                                ReturnedWelcomeMessageLog =
                                                    x.LeaversLogChannelMessage
                                                    |> Option.map (fun x -> x.Message)
                                                    |> Option.map EnabledOptionValue.Init
                                                    |> Option.defaultValue EnabledOptionValue.Empty
                                            }
                                        Exit =
                                            { data.Exit with
                                                Channel =
                                                    x.OutputLeaveChannel
                                                    |> Option.map EnabledOptionValue.Init
                                                    |> Option.defaultValue EnabledOptionValue.Empty
                                                GoodbyeMessage =
                                                    x.TemplateLeaveMessage
                                                    |> Option.map EnabledOptionValue.Init
                                                    |> Option.defaultValue EnabledOptionValue.Empty
                                            }
                                    }
                                )
                        )
                        guildData

                NewcomersRolesOld.GuildNewcomersRoles.drop db permissiveRoles |> ignore
                WelcomeSettingOld.GuildWelcomeSetting.drop db templateRoles |> ignore

                guildData
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
                                        Serialization.BsonSerializer.Deserialize<Data<MainData>>(x)
                                    | x ->
                                        failwithf "Version = %A not implemented" x

                                Map.add x.GuildId x st
                            )
                            Map.empty
                    Collection = collection
                }

        let drop (db: IMongoDatabase) (guildWelcomeSetting: GuildData) =
            db.DropCollection collectionName

            { guildWelcomeSetting with
                Cache = Map.empty
            }

        let tryFind guildId (guildWelcomeSetting: GuildData) =
            Map.tryFind guildId guildWelcomeSetting.Cache
            |> Option.map (fun x -> x.Data)

module InvitesSetting =
    type SettingData =
        {
            mutable Id: ObjectId
            mutable GuildId: GuildId
            mutable OutputChannel: ChannelId
            /// `invite_code * message`
            mutable Associations: (string * string) []
        }
        static member Init(guildId, outputChannel, associations) =
            {
                Id = ObjectId.Empty
                GuildId = guildId
                OutputChannel = outputChannel
                Associations = associations
            }

    let welcomeSetting = Db.database.GetCollection<SettingData>("invitesSetting")

    type GuildSetting = Map<GuildId, SettingData>

    let getAll (): GuildSetting =
        welcomeSetting.Find(fun x -> true).ToEnumerable()
        |> Seq.fold
            (fun st x ->
                Map.add x.GuildId x st
            )
            Map.empty

    let replace (newData: SettingData) =
        welcomeSetting.ReplaceOne((fun x -> x.Id = newData.Id), newData)
        |> ignore

    let insert (guildId, outputChannel, associations) =
        let x = SettingData.Init(guildId, outputChannel, associations)
        welcomeSetting.InsertOne(x)
        x

module Leavers =
    type Data =
        {
            mutable Id: ObjectId
            mutable GuildId: GuildId
            mutable UserId: UserId
            mutable RoleIds: RoleId []
        }
        static member Init(guildId, userId, rolesId) =
            {
                Id = ObjectId.Empty
                GuildId = guildId
                UserId = userId
                RoleIds = rolesId
            }

    let datas = Db.database.GetCollection<Data>("leavers")

    type GuildDatas = Map<GuildId, Map<UserId, Data>>

    let getAll (): GuildDatas =
        datas.Find(fun x -> true).ToEnumerable()
        |> Seq.fold
            (fun st x ->
                st
                |> Map.addOrModWith
                    x.GuildId
                    (fun () -> Map.add x.UserId x Map.empty)
                    (fun st -> Map.add x.UserId x st)
            )
            Map.empty

    let replace (newRoleData: Data) =
        datas.ReplaceOne((fun x -> x.Id = newRoleData.Id), newRoleData)
        |> ignore

    let insert (guildId, userId, rolesId) =
        let x = Data.Init(guildId, userId, rolesId)
        datas.InsertOne(x)
        x

    let remove (roleData: Data) =
        datas.DeleteOne(fun x -> x.Id = roleData.Id)
        |> ignore

type State =
    {
        Settings: Setting.GuildData
        Leavers: Leavers.GuildDatas
    }
