module Doorkeeper.Model
open FsharpMyExtension
open MongoDB.Driver
open MongoDB.Bson

open Types

type Settings = Map<GuildId, RoleId []>

module NewcomersRoles =
    type PassSettings =
        {
            PermittedRoles: RoleId Set
            MainChannelId: ChannelId
            WelcomeMessage: string
        }
        static member SampleJson =
            {
                PermittedRoles = Set [ 12345678UL ]
                MainChannelId = 124576UL
                WelcomeMessage = "<@userName>, welcome to the club!"
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
        }
        static member Init(guildId, outputChannel, roleIds) =
            {
                Id = ObjectId.Empty
                GuildId = guildId
                OutputChannel = outputChannel
                TemplateMessage = roleIds
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

    let insert (guildId, outputChannel, roleIds) =
        let x = WelcomeSettingData.Init(guildId, outputChannel, roleIds)
        welcomeSetting.InsertOne(x)
        x
