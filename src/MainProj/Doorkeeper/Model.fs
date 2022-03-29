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
        }
        static member Init(guildId, outputChannel, templateMessage, outputLogChannel, templateLogMessage, outputLeaveChannel, templateLeaveMessage) =
            {
                Id = ObjectId.Empty
                GuildId = guildId
                OutputChannel = outputChannel
                TemplateMessage = templateMessage
                OutputLogChannel = outputLogChannel
                TemplateLogMessage = templateLogMessage
                OutputLeaveChannel = outputLeaveChannel
                TemplateLeaveMessage = templateLeaveMessage
            }

    let welcomeSetting = Db.database.GetCollection<WelcomeSettingData>("welcomeSetting")

    type GuildWelcomeSetting = Map<GuildId, WelcomeSettingData>

    let getAll (): GuildWelcomeSetting =
        welcomeSetting.Find(fun x -> true).ToEnumerable()
        |> Seq.fold
            (fun st x ->
                Map.add x.GuildId x st
            )
            Map.empty

    let replace (newData: WelcomeSettingData) =
        welcomeSetting.ReplaceOne((fun x -> x.Id = newData.Id), newData)
        |> ignore

    let insert (guildId, outputChannel, roleIds, outputLogChannel, templateLogMessage, outputLeaveChannel, templateLeaveMessage) =
        let x = WelcomeSettingData.Init(guildId, outputChannel, roleIds, outputLogChannel, templateLogMessage, outputLeaveChannel, templateLeaveMessage)
        welcomeSetting.InsertOne(x)
        x

module InvitesSetting =
    type SettingData =
        {
            mutable Id: ObjectId
            mutable GuildId: GuildId
            mutable OutputChannel: ChannelId
        }
        static member Init(guildId, outputChannel) =
            {
                Id = ObjectId.Empty
                GuildId = guildId
                OutputChannel = outputChannel
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

    let insert (guildId, outputChannel) =
        let x = SettingData.Init(guildId, outputChannel)
        welcomeSetting.InsertOne(x)
        x
