module Doorkeeper.Api
open Shared.Api
open Shared.MessageTemplate
open Types
open Model

type RoleId = Snowflake
type ChannelId = Snowflake
type GuildId = Snowflake

module TransferTypes =
    type Optional<'a> = Option<'a>

    let ofSnowflake =
        Option.map (EnabledOptionValue.map (fun x -> x.v))

    let toSnowflake =
        EnabledOptionValue.map Snowflake.Create

    module UpdateDb =
        let convert newData oldData =
            newData |> ofSnowflake |> Option.defaultValue oldData

        let convertMessage newData oldData =
            newData |> Option.defaultValue oldData

        let convertArray newData oldData =
            let ofSnowflake =
                Option.map (EnabledOptionValue.map (Array.map (fun (x, y) -> x, y.v)))

            newData |> ofSnowflake |> Option.defaultValue oldData

    module OfDb =
        let convert newData =
            newData |> toSnowflake |> Some

        let convertMessage newData =
            newData |> Some

        let convertArray newData =
            let toSnowflake =
                EnabledOptionValue.map (Array.map (fun (x, y) -> x, Snowflake.Create y))

            newData |> toSnowflake |> Some

    type Checkpoint =
        {
            Channel: Optional<EnabledOptionValue<ChannelId>>
            DoorkeeperRole: Optional<EnabledOptionValue<RoleId>>
            /// dictionary key is the role name that is specified in the pass command
            IssuedRoleIds: Optional<EnabledOptionValue<(string * RoleId) []>>
            EnteredUserRole: Optional<EnabledOptionValue<RoleId>>
            NewcomerWelcomeMessage: Optional<EnabledOptionValue<MessageRaw []>>
            NewcomerWelcomeMessageLog: Optional<EnabledOptionValue<MessageRaw>>
            ReturnedWelcomeMessage: Optional<EnabledOptionValue<MessageRaw []>>
            ReturnedWelcomeMessageLog: Optional<EnabledOptionValue<MessageRaw>>
            GoodbyeMessage: Optional<EnabledOptionValue<MessageRaw []>>
            GoodbyeMessageLog: Optional<EnabledOptionValue<MessageRaw>>
        }
        static member UpdateDbData(newData: Checkpoint, oldData: Setting.Checkpoint): Setting.Checkpoint =
            {
                Channel = UpdateDb.convert newData.Channel oldData.Channel
                DoorkeeperRole = UpdateDb.convert newData.DoorkeeperRole oldData.DoorkeeperRole
                IssuedRoleIds = UpdateDb.convertArray newData.IssuedRoleIds oldData.IssuedRoleIds
                EnteredUserRole = UpdateDb.convert newData.EnteredUserRole oldData.EnteredUserRole
                NewcomerWelcomeMessage = UpdateDb.convertMessage newData.NewcomerWelcomeMessage oldData.NewcomerWelcomeMessage
                NewcomerWelcomeMessageLog = UpdateDb.convertMessage newData.NewcomerWelcomeMessageLog oldData.NewcomerWelcomeMessageLog
                ReturnedWelcomeMessage = UpdateDb.convertMessage newData.ReturnedWelcomeMessage oldData.ReturnedWelcomeMessage
                ReturnedWelcomeMessageLog = UpdateDb.convertMessage newData.ReturnedWelcomeMessageLog oldData.ReturnedWelcomeMessageLog
                GoodbyeMessage = UpdateDb.convertMessage newData.GoodbyeMessage oldData.GoodbyeMessage
                GoodbyeMessageLog = UpdateDb.convertMessage newData.GoodbyeMessageLog oldData.GoodbyeMessageLog
            }
        static member OfDbData(newData: Setting.Checkpoint): Checkpoint =
            {
                Channel = OfDb.convert newData.Channel
                DoorkeeperRole = OfDb.convert newData.DoorkeeperRole
                IssuedRoleIds = OfDb.convertArray newData.IssuedRoleIds
                EnteredUserRole = OfDb.convert newData.EnteredUserRole
                NewcomerWelcomeMessage = OfDb.convertMessage newData.NewcomerWelcomeMessage
                NewcomerWelcomeMessageLog = OfDb.convertMessage newData.NewcomerWelcomeMessageLog
                ReturnedWelcomeMessage = OfDb.convertMessage newData.ReturnedWelcomeMessage
                ReturnedWelcomeMessageLog = OfDb.convertMessage newData.ReturnedWelcomeMessageLog
                GoodbyeMessage = OfDb.convertMessage newData.GoodbyeMessage
                GoodbyeMessageLog = OfDb.convertMessage newData.GoodbyeMessageLog
            }

    type Inner =
        {
            Channel: Optional<EnabledOptionValue<ChannelId>>
            NewcomerWelcomeMessage: Optional<EnabledOptionValue<MessageRaw []>>
            NewcomerWelcomeMessageLog: Optional<EnabledOptionValue<MessageRaw>>
            ReturnedWelcomeMessage: Optional<EnabledOptionValue<MessageRaw []>>
            ReturnedWelcomeMessageLog: Optional<EnabledOptionValue<MessageRaw>>
        }
        static member UpdateDbData(newData: Inner, oldData: Setting.Inner): Setting.Inner =
            {
                Channel = UpdateDb.convert newData.Channel oldData.Channel
                NewcomerWelcomeMessage = UpdateDb.convertMessage newData.NewcomerWelcomeMessage oldData.NewcomerWelcomeMessage
                NewcomerWelcomeMessageLog = UpdateDb.convertMessage newData.NewcomerWelcomeMessageLog oldData.NewcomerWelcomeMessageLog
                ReturnedWelcomeMessage = UpdateDb.convertMessage newData.ReturnedWelcomeMessage oldData.ReturnedWelcomeMessage
                ReturnedWelcomeMessageLog = UpdateDb.convertMessage newData.ReturnedWelcomeMessageLog oldData.ReturnedWelcomeMessageLog
            }
        static member OfDbData(newData: Setting.Inner) =
            {
                Channel = OfDb.convert newData.Channel
                NewcomerWelcomeMessage = OfDb.convertMessage newData.NewcomerWelcomeMessage
                NewcomerWelcomeMessageLog = OfDb.convertMessage newData.NewcomerWelcomeMessageLog
                ReturnedWelcomeMessage = OfDb.convertMessage newData.ReturnedWelcomeMessage
                ReturnedWelcomeMessageLog = OfDb.convertMessage newData.ReturnedWelcomeMessageLog
            }

    type Exit =
        {
            Channel: Optional<EnabledOptionValue<ChannelId>>
            GoodbyeMessage: Optional<EnabledOptionValue<MessageRaw []>>
            GoodbyeMessageLog: Optional<EnabledOptionValue<MessageRaw>>
        }
        static member UpdateDbData(newData: Exit, oldData: Setting.Exit): Setting.Exit =
            {
                Channel = UpdateDb.convert newData.Channel oldData.Channel
                GoodbyeMessage = UpdateDb.convertMessage newData.GoodbyeMessage oldData.GoodbyeMessage
                GoodbyeMessageLog = UpdateDb.convertMessage newData.GoodbyeMessageLog oldData.GoodbyeMessageLog
            }
        static member OfDbData(newData: Setting.Exit): Exit =
            {
                Channel = OfDb.convert newData.Channel
                GoodbyeMessage = OfDb.convertMessage newData.GoodbyeMessage
                GoodbyeMessageLog = OfDb.convertMessage newData.GoodbyeMessageLog
            }

    type Log =
        {
            Channel: Optional<EnabledOptionValue<ChannelId>>
        }
        static member UpdateDbData(newData: Log, oldData: Setting.Log): Setting.Log =
            {
                Channel = UpdateDb.convert newData.Channel oldData.Channel
            }
        static member OfDbData(newData: Setting.Log): Log =
            {
                Channel = OfDb.convert newData.Channel
            }

    type MainData =
        {
            Checkpoint: Optional<Checkpoint>
            Inner: Optional<Inner>
            Exit: Optional<Exit>
            Log: Optional<Log>
        }
        static member UpdateDbData(newData: MainData, oldData: Setting.MainData): Setting.MainData =
            let convert newData upd oldData  =
                newData |> Option.map (fun x -> upd(x, oldData)) |> Option.defaultValue oldData

            {
                Checkpoint = convert newData.Checkpoint Checkpoint.UpdateDbData oldData.Checkpoint
                Inner = convert newData.Inner Inner.UpdateDbData oldData.Inner
                Exit = convert newData.Exit Exit.UpdateDbData oldData.Exit
                Log = convert newData.Log Log.UpdateDbData oldData.Log
            }
        static member OfDbData(newData: Setting.MainData) =
            let convert x upd = upd x |> Some

            {
                Checkpoint = convert newData.Checkpoint Checkpoint.OfDbData
                Inner = convert newData.Inner Inner.OfDbData
                Exit = convert newData.Exit Exit.OfDbData
                Log = convert newData.Log Log.OfDbData
            }

let set (guildId: GuildId, newData) (state: State) =
    let settings =
        state.Settings
        |> Setting.GuildData.set guildId.v (fun old ->
            TransferTypes.MainData.UpdateDbData(newData, old)
        )

    { state with Settings = settings }

let get (guildId: GuildId) (state: State) =
    Setting.GuildData.tryFind guildId.v state.Settings
    |> Option.map TransferTypes.MainData.OfDbData

type Request =
    | Get of GuildId
    | Set of GuildId * TransferTypes.MainData

type Response =
    | Set of Result<unit, string>
    | Get of TransferTypes.MainData option

let requestHandler (state: State) (r: Request): option<_> * Response =
    match r with
    | Request.Set (guildId, newData) ->
        let state =
            set (guildId, newData) state

        (Some state, Ok ())
        |> fun (state, x) -> state, Response.Set x

    | Request.Get guildId ->
        let settings =
            get (guildId: GuildId) state

        None, Response.Get settings
