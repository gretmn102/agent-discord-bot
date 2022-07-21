module Doorkeeper.Model
open FsharpMyExtension
open MongoDB.Driver
open MongoDB.Bson

open Types

module NewcomersRoles =
    type PassSettings =
        {
            PermittedRoles: RoleId Set
            MainChannelId: ChannelId
            WelcomeMessage: string
            /// dictionary key is the role name that is specified in the pass command
            IssuedRoleIds: (string * RoleId) []
            PassLogMessage: (ChannelId * string) option
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
        static member Init(guildId: GuildId, roleIds: RoleId Set, passSettings) =
            {
                Id = ObjectId.Empty
                GuildId = guildId
                RoleIds = roleIds

                PassSettings = passSettings
            }

    let permissiveRoles = Db.database.GetCollection<NewcomersRolesData>("newcomersRoles")

    type GuildNewcomersRoles = Map<GuildId, NewcomersRolesData>

    let getAll (): GuildNewcomersRoles =
        permissiveRoles.Find(fun x -> true).ToEnumerable()
        |> Seq.fold
            (fun st x ->
                Map.add x.GuildId x st
            )
            Map.empty

    let replace (newData: NewcomersRolesData) =
        permissiveRoles.ReplaceOne((fun x -> x.Id = newData.Id), newData)
        |> ignore

    let insert (guildId: GuildId, roleIds: RoleId Set, passSettings) =
        let x = NewcomersRolesData.Init(guildId, roleIds, passSettings)
        permissiveRoles.InsertOne(x)
        x

module WelcomeSetting =
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
        let getAll (db: IMongoDatabase): GuildWelcomeSetting =
            let welcomeSetting = db.GetCollection<WelcomeSettingData>("welcomeSetting")

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
        NewcomersRoles: NewcomersRoles.GuildNewcomersRoles
        WelcomeSetting: WelcomeSetting.GuildWelcomeSetting
        Leavers: Leavers.GuildDatas
    }
