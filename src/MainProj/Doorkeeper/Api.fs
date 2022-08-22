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
                    state.WelcomeSetting
                    |> WelcomeSetting.GuildWelcomeSetting.setWelcomeSetting guildId.v (fun x ->
                        { x with OutputChannel = Some channelId.v }
                    )

                Some { state with WelcomeSetting = state' }, Ok ()

            | Checkpoint.DoorkeeperRole roleId ->
                let state' =
                    state.NewcomersRoles
                    |> NewcomersRoles.GuildNewcomersRoles.setWelcomeSetting guildId.v (fun x ->
                        { x with
                            PassSettings =
                                let passSettings =
                                    x.PassSettings
                                    |> Option.defaultValue NewcomersRoles.PassSettings.Empty

                                { passSettings with
                                    PermittedRoles = Set.singleton roleId.v
                                }
                                |> Some
                        }
                    )

                Some { state with NewcomersRoles = state' }, Ok ()

            | Checkpoint.EnteredUserRole roleId ->
                let state' =
                    state.NewcomersRoles
                    |> NewcomersRoles.GuildNewcomersRoles.setWelcomeSetting guildId.v (fun x ->
                        { x with RoleIds = Set.singleton roleId.v }
                    )

                Some { state with NewcomersRoles = state' }, Ok ()

            | Checkpoint.NewcomerWelcomeMessage msg ->
                // TODO: check if the message is correct

                let state' =
                    state.WelcomeSetting
                    |> WelcomeSetting.GuildWelcomeSetting.setWelcomeSetting guildId.v (fun x ->
                        { x with TemplateMessage = Some msg }
                    )

                Some { state with WelcomeSetting = state' }, Ok ()

            | Checkpoint.ReturnedWelcomeMessage msg ->
                // TODO: check if the message is correct

                match WelcomeSetting.GuildWelcomeSetting.tryFind guildId.v state.WelcomeSetting with
                | Some welcomeSetting ->
                    let channelId =
                        welcomeSetting.OutputChannel
                        |> Option.orElseWith (fun () ->
                            welcomeSetting.LeaversChannelMessage
                            |> Option.map (fun x -> x.ChannelId)
                        )

                    match channelId with
                    | Some channelId ->
                        let state' =
                            state.WelcomeSetting
                            |> WelcomeSetting.GuildWelcomeSetting.setWelcomeSetting guildId.v (fun x ->
                                { x with
                                    LeaversChannelMessage =
                                        WelcomeSetting.ChannelMessage.Init channelId msg
                                        |> Some
                                }
                            )

                        Some { state with WelcomeSetting = state' }, Ok ()

                    | None ->
                        None, Error "Checkpoint.Channel is None"
                | None ->
                    None, Error "Checkpoint.Channel is None"

        | Inner x ->
            match x with
            | Inner.Channel channelId ->
                let state' =
                    state.NewcomersRoles
                    |> NewcomersRoles.GuildNewcomersRoles.setWelcomeSetting guildId.v (fun x ->
                        { x with
                            PassSettings =
                                let passSettings =
                                    x.PassSettings
                                    |> Option.defaultValue NewcomersRoles.PassSettings.Empty

                                { passSettings with
                                    MainChannelId = channelId.v
                                }
                                |> Some
                        }
                    )

                Some { state with NewcomersRoles = state' }, Ok ()

            | Inner.NewcomerWelcomeMessage msg ->
                // TODO: check if the message is correct

                let state' =
                    state.NewcomersRoles
                    |> NewcomersRoles.GuildNewcomersRoles.setWelcomeSetting guildId.v (fun x ->
                        { x with
                            PassSettings =
                                let passSettings =
                                    x.PassSettings
                                    |> Option.defaultValue NewcomersRoles.PassSettings.Empty

                                { passSettings with
                                    WelcomeMessage = msg
                                }
                                |> Some
                        }
                    )

                Some { state with NewcomersRoles = state' }, Ok ()

        | Exit x ->
            match x with
            | Exit.Channel channelId ->
                let state' =
                    state.WelcomeSetting
                    |> WelcomeSetting.GuildWelcomeSetting.setWelcomeSetting guildId.v (fun x ->
                        { x with OutputLeaveChannel = Some channelId.v }
                    )

                Some { state with WelcomeSetting = state' }, Ok ()
            | Exit.GoodbyeMessage msg ->
                // TODO: check if the message is correct

                let state' =
                    state.WelcomeSetting
                    |> WelcomeSetting.GuildWelcomeSetting.setWelcomeSetting guildId.v (fun x ->
                        { x with TemplateLeaveMessage = Some msg }
                    )

                Some { state with WelcomeSetting = state' }, Ok ()

        |> fun (state, x) -> state, Response.Set x

    | Request.Get guildId ->
        let welcomeSetting = WelcomeSetting.GuildWelcomeSetting.tryFind guildId.v state.WelcomeSetting
        let mewcomersRolesSetting = NewcomersRoles.GuildNewcomersRoles.tryFind guildId.v state.NewcomersRoles

        {
            Checkpoint =
                {|
                    Channel = welcomeSetting |> Option.bind (fun x -> x.OutputChannel |> Option.map Snowflake.Create)
                    DoorkeeperRole =
                        mewcomersRolesSetting
                        |> Option.bind (fun x ->
                            x.PassSettings
                            |> Option.bind (fun x ->
                                x.PermittedRoles
                                |> Set.tryFirst
                                |> Option.map Snowflake.Create
                            )
                        )
                    EnteredUserRole =
                        mewcomersRolesSetting
                        |> Option.bind (fun x ->
                            x.RoleIds
                            |> Set.tryFirst
                            |> Option.map Snowflake.Create
                        )
                    NewcomerWelcomeMessage = welcomeSetting |> Option.bind (fun x -> x.TemplateMessage)
                    ReturnedWelcomeMessage =
                        welcomeSetting
                        |> Option.bind (fun x ->
                            x.LeaversChannelMessage |> Option.map (fun x -> x.Message))
                |}
            Inner =
                {|
                    Channel =
                        mewcomersRolesSetting
                        |> Option.bind (fun x ->
                            x.PassSettings
                            |> Option.map (fun x ->
                                x.MainChannelId
                                |> Snowflake.Create
                            )
                        )
                    NewcomerWelcomeMessage =
                        mewcomersRolesSetting
                        |> Option.bind (fun x ->
                            x.PassSettings
                            |> Option.map (fun x ->
                                x.WelcomeMessage
                            )
                        )
                    // ReturnedWelcomeMessage = MessageTemplate
                |}
            Exit =
                {|
                    Channel = welcomeSetting |> Option.bind (fun x -> x.OutputLeaveChannel |> Option.map Snowflake.Create)
                    GoodbyeMessage = welcomeSetting |> Option.bind (fun x -> x.TemplateLeaveMessage)
                |}
        }
        |> fun res -> None, Response.Get res
