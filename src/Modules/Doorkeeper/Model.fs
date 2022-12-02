module Doorkeeper.Model
open FsharpMyExtension
open MongoDB.Driver
open MongoDB.Bson
open MongoDB.Bson.Serialization.Attributes

open Types
open Shared.MessageTemplate
open Db

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
            EnteredUserRole: EnabledOptionValue<RoleId>
            NewcomerWelcomeMessage: EnabledOptionValue<MessageRaw []>
            NewcomerWelcomeMessageLog: EnabledOptionValue<MessageRaw>
            /// a list of roles that need to be returned to the returned user if he had such
            ReturnedUserIncludeRoles: EnabledOptionValue<RoleId []>
            ReturnedWelcomeMessage: EnabledOptionValue<MessageRaw []>
            ReturnedWelcomeMessageLog: EnabledOptionValue<MessageRaw>
            GoodbyeMessage: EnabledOptionValue<MessageRaw []>
            GoodbyeMessageLog: EnabledOptionValue<MessageRaw>
        }
        static member Empty =
            {
                Channel = EnabledOptionValue.Empty
                DoorkeeperRole = EnabledOptionValue.Empty
                IssuedRoleIds = EnabledOptionValue.Empty
                EnteredUserRole = EnabledOptionValue.Empty
                NewcomerWelcomeMessage = EnabledOptionValue.Empty
                NewcomerWelcomeMessageLog = EnabledOptionValue.Empty
                ReturnedUserIncludeRoles = EnabledOptionValue.Empty
                ReturnedWelcomeMessage = EnabledOptionValue.Empty
                ReturnedWelcomeMessageLog = EnabledOptionValue.Empty
                GoodbyeMessage = EnabledOptionValue.Empty
                GoodbyeMessageLog = EnabledOptionValue.Empty
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
                    123456789UL
                    |> EnabledOptionValue.Init
                NewcomerWelcomeMessage =
                    "<@userMention>, welcome to our guild!"
                    |> Array.singleton
                    |> EnabledOptionValue.Init
                NewcomerWelcomeMessageLog =
                    "<@userMention> entered to guild!"
                    |> EnabledOptionValue.Init
                ReturnedUserIncludeRoles =
                    EnabledOptionValue.Empty
                ReturnedWelcomeMessage =
                    "<@userMention>, welcome back!"
                    |> Array.singleton
                    |> EnabledOptionValue.Init
                ReturnedWelcomeMessageLog =
                    "<@userMention> returned to us"
                    |> EnabledOptionValue.Init
                GoodbyeMessage =
                    "<@userMention> has left"
                    |> Array.singleton
                    |> EnabledOptionValue.Init
                GoodbyeMessageLog =
                    "<@userName> has left"
                    |> EnabledOptionValue.Init
            }

    type Inner =
        {
            Channel: EnabledOptionValue<ChannelId>
            NewcomerWelcomeMessage: EnabledOptionValue<MessageRaw []>
            NewcomerWelcomeMessageLog: EnabledOptionValue<MessageRaw>
            ReturnedUserExcludeRoles: EnabledOptionValue<RoleId []>
            ReturnedWelcomeMessage: EnabledOptionValue<MessageRaw []>
            ReturnedWelcomeMessageLog: EnabledOptionValue<MessageRaw>
        }
        static member Empty =
            {
                Channel = EnabledOptionValue.Empty
                NewcomerWelcomeMessage = EnabledOptionValue.Empty
                NewcomerWelcomeMessageLog = EnabledOptionValue.Empty
                ReturnedUserExcludeRoles = EnabledOptionValue.Empty
                ReturnedWelcomeMessage = EnabledOptionValue.Empty
                ReturnedWelcomeMessageLog = EnabledOptionValue.Empty
            }
        static member Sample =
            {
                Channel =
                    124576UL
                    |> EnabledOptionValue.Init
                NewcomerWelcomeMessage =
                    "<@userMention>, welcome to the club!"
                    |> Array.singleton
                    |> EnabledOptionValue.Init
                NewcomerWelcomeMessageLog =
                    "<@userName> is let in"
                    |> EnabledOptionValue.Init
                ReturnedUserExcludeRoles =
                    EnabledOptionValue.Empty
                ReturnedWelcomeMessage =
                    "<@userMention> returned to the club!"
                    |> Array.singleton
                    |> EnabledOptionValue.Init
                ReturnedWelcomeMessageLog =
                    "<@userName> is let in"
                    |> EnabledOptionValue.Init
            }


    type Exit =
        {
            Channel: EnabledOptionValue<ChannelId>
            GoodbyeMessage: EnabledOptionValue<MessageRaw []>
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
                    "<@userMention> has left"
                    |> Array.singleton
                    |> EnabledOptionValue.Init
                GoodbyeMessageLog =
                    "<@userName> has left"
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
                                                    |> Seq.tryHead
                                                    |> Option.map EnabledOptionValue.Init
                                                    |> Option.defaultValue EnabledOptionValue.Empty
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
                                                    |> Option.map (fun x -> x.WelcomeMessage |> Array.singleton)
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
                                                    |> Option.map Array.singleton
                                                    |> Option.map EnabledOptionValue.Init
                                                    |> Option.defaultValue EnabledOptionValue.Empty
                                                NewcomerWelcomeMessageLog =
                                                    x.TemplateLogMessage
                                                    |> Option.map EnabledOptionValue.Init
                                                    |> Option.defaultValue EnabledOptionValue.Empty
                                                ReturnedWelcomeMessage =
                                                    x.LeaversChannelMessage
                                                    |> Option.map (fun x -> x.Message)
                                                    |> Option.map Array.singleton
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
                                                    |> Option.map Array.singleton
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
    [<BsonIgnoreExtraElements>]
    type DataPreVersion =
        {
            Id: ObjectId
            GuildId: GuildId
            OutputChannel: ChannelId
            /// `invite_code * message`
            Associations: (string * string) []
        }
        static member Init(guildId, outputChannel, associations) =
            {
                Id = ObjectId.Empty
                GuildId = guildId
                OutputChannel = outputChannel
                Associations = associations
            }

    type GuildData =
        {
            OutputChannel: ChannelId
            /// `invite_code * message`
            Associations: (string * string) []
        }
        static member Init outputChannel associations =
            {
                OutputChannel = outputChannel
                Associations = associations
            }
        static member Empty =
            {
                OutputChannel = 0UL
                Associations = [||]
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

    type Id = GuildId

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
                            GuildData.Init oldValue.OutputChannel oldValue.Associations
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

module Leavers =
    [<BsonIgnoreExtraElements>]
    type DataPreVersion =
        {
            Id: ObjectId
            GuildId: GuildId
            UserId: UserId
            RoleIds: RoleId []
        }

    type GuildUserData =
        {
            RoleIds: RoleId []
        }
        static member Init roleIds =
            {
                RoleIds = roleIds
            }
        static member Empty =
            {
                RoleIds = [||]
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
                            GuildUserData.Init oldValue.RoleIds
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

type State =
    {
        Settings: Setting.GuildData
        Leavers: Leavers.GuildUsers
    }
