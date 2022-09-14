module Doorkeeper.Api
open Shared
open Types
open Model

type Message = string
type RoleId = Snowflake
type ChannelId = Snowflake
type GuildId = Snowflake

type Checkpoint =
    | Channel of ChannelId
    | DoorkeeperRole of RoleId
    | EnteredUserRole of RoleId
    | NewcomerWelcomeMessage of Message
    | ReturnedWelcomeMessage of Message

type Inner =
    | Channel of ChannelId
    | NewcomerWelcomeMessage of Message
    // | ReturnedWelcomeMessage of Message

type Exit =
    | Channel of ChannelId
    | GoodbyeMessage of Message

type SetRoot =
    | Checkpoint of Checkpoint
    | Inner of Inner
    | Exit of Exit

type Request =
    | Get of GuildId
    | Set of GuildId * SetRoot

type Settings =
    {
        Checkpoint:
            {|
                Channel: ChannelId option
                DoorkeeperRole: RoleId option
                EnteredUserRole: RoleId option
                NewcomerWelcomeMessage: Message option
                ReturnedWelcomeMessage: Message option
            |}
        Inner:
            {|
                Channel: ChannelId option
                NewcomerWelcomeMessage: Message option
                // ReturnedWelcomeMessage: Message option
            |}
        Exit:
            {|
                Channel: ChannelId option
                GoodbyeMessage: Message option
            |}
    }

type Response =
    | Set of Result<unit, string>
    | Get of Settings

module Set =
    let tryFirst (s: _ Set) =
        if Set.isEmpty s then
            None
        else
            Some (Seq.head s)

let requestHandler (state: State) (r: Request): option<_> * Response =
    match r with
    | Request.Set (guildId, x) ->
        match x with
        | Checkpoint x ->
            match x with
            | Checkpoint.Channel channelId ->
                let state' =
                    state.Settings
                    |> Setting.GuildData.set guildId.v (fun x ->
                        { x with
                            Checkpoint = {
                                x.Checkpoint with
                                    Channel = EnabledOptionValue.Init channelId.v
                            }
                        }
                    )

                Some { state with Settings = state' }, Ok ()

            | Checkpoint.DoorkeeperRole roleId ->
                let state' =
                    state.Settings
                    |> Setting.GuildData.set guildId.v (fun x ->
                        { x with
                            Checkpoint = {
                                x.Checkpoint with
                                    DoorkeeperRole = EnabledOptionValue.Init roleId.v
                            }
                        }
                    )

                Some { state with Settings = state' }, Ok ()

            | Checkpoint.EnteredUserRole roleId ->
                let state' =
                    state.Settings
                    |> Setting.GuildData.set guildId.v (fun x ->
                        { x with
                            Checkpoint = {
                                x.Checkpoint with
                                    EnteredUserRole = EnabledOptionValue.Init roleId.v
                            }
                        }
                    )

                Some { state with Settings = state' }, Ok ()

            | Checkpoint.NewcomerWelcomeMessage msg ->
                // TODO: check if the message is correct

                let state' =
                    state.Settings
                    |> Setting.GuildData.set guildId.v (fun x ->
                        { x with
                            Checkpoint = {
                                x.Checkpoint with
                                    NewcomerWelcomeMessage = EnabledOptionValue.Init msg
                            }
                        }
                    )

                Some { state with Settings = state' }, Ok ()

            | Checkpoint.ReturnedWelcomeMessage msg ->
                // TODO: check if the message is correct

                let state' =
                    state.Settings
                    |> Setting.GuildData.set guildId.v (fun x ->
                        { x with
                            Checkpoint = {
                                x.Checkpoint with
                                    ReturnedWelcomeMessage = EnabledOptionValue.Init msg
                            }
                        }
                    )

                Some { state with Settings = state' }, Ok ()

        | Inner x ->
            match x with
            | Inner.Channel channelId ->
                let state' =
                    state.Settings
                    |> Setting.GuildData.set guildId.v (fun x ->
                        { x with
                            Inner = {
                                x.Inner with
                                    Channel = EnabledOptionValue.Init channelId.v
                            }
                        }
                    )

                Some { state with Settings = state' }, Ok ()

            | Inner.NewcomerWelcomeMessage msg ->
                // TODO: check if the message is correct

                let state' =
                    state.Settings
                    |> Setting.GuildData.set guildId.v (fun x ->
                        { x with
                            Inner = {
                                x.Inner with
                                    NewcomerWelcomeMessage = EnabledOptionValue.Init msg
                            }
                        }
                    )

                Some { state with Settings = state' }, Ok ()

        | Exit x ->
            match x with
            | Exit.Channel channelId ->
                let state' =
                    state.Settings
                    |> Setting.GuildData.set guildId.v (fun x ->
                        { x with
                            Exit = {
                                x.Exit with
                                    Channel = EnabledOptionValue.Init channelId.v
                            }
                        }
                    )

                Some { state with Settings = state' }, Ok ()

            | Exit.GoodbyeMessage msg ->
                // TODO: check if the message is correct

                let state' =
                    state.Settings
                    |> Setting.GuildData.set guildId.v (fun x ->
                        { x with
                            Exit = {
                                x.Exit with
                                    GoodbyeMessage = EnabledOptionValue.Init msg
                            }
                        }
                    )

                Some { state with Settings = state' }, Ok ()

        |> fun (state, x) -> state, Response.Set x

    | Request.Get guildId ->
        let settings = Setting.GuildData.tryFind guildId.v state.Settings

        {
            Checkpoint =
                {|
                    Channel =
                        settings |> Option.bind (fun x -> x.Checkpoint.Channel.Value |> Option.map Snowflake.Create)

                    DoorkeeperRole =
                        settings |> Option.bind (fun x -> x.Checkpoint.DoorkeeperRole.Value |> Option.map Snowflake.Create)

                    EnteredUserRole =
                        settings
                        |> Option.bind (fun x ->
                            x.Checkpoint.EnteredUserRole.Value
                            |> Option.map Snowflake.Create
                        )

                    NewcomerWelcomeMessage =
                        settings |> Option.bind (fun x -> x.Checkpoint.NewcomerWelcomeMessage.Value)

                    ReturnedWelcomeMessage =
                        settings |> Option.bind (fun x -> x.Checkpoint.ReturnedWelcomeMessage.Value)
                |}
            Inner =
                {|
                    Channel =
                        settings |> Option.bind (fun x -> x.Inner.Channel.Value |> Option.map Snowflake.Create)

                    NewcomerWelcomeMessage =
                        settings |> Option.bind (fun x -> x.Inner.NewcomerWelcomeMessage.Value)
                    // ReturnedWelcomeMessage = MessageTemplate
                |}
            Exit =
                {|
                    Channel =
                        settings |> Option.bind (fun x -> x.Exit.Channel.Value |> Option.map Snowflake.Create)

                    GoodbyeMessage =
                        settings |> Option.bind (fun x -> x.Exit.GoodbyeMessage.Value)
                |}
        }
        |> fun res -> None, Response.Get res
