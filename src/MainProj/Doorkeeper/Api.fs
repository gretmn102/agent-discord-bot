module Doorkeeper.Api
open Shared
open Types
open Model

type Message = string

type Checkpoint =
    | Channel of ChannelId
    // | DoorkeeperRole of RoleId
    // | EnteredUserRole of RoleId
    | NewcomerWelcomeMessage of Message
    | ReturnedWelcomeMessage of Message

// type Inner =
//     | Channel of ChannelId
//     | NewcomerWelcomeMessage of Message
//     | ReturnedWelcomeMessage of Message

type Exit =
    | Channel of ChannelId
    | GoodbyeMessage of Message

type SetRoot =
    | Checkpoint of Checkpoint
    // | Inner of Inner
    | Exit of Exit

type Request =
    | Get of GuildId
    | Set of GuildId * SetRoot

type Settings =
    {
        Checkpoint:
            {|
                Channel: ChannelId option
                // DoorkeeperRole: RoleId option
                // EnteredUserRole: RoleId option
                NewcomerWelcomeMessage: Message option
                ReturnedWelcomeMessage: Message option
            |}
        // Inner:
        //     {|
        //         Channel: ChannelId option
        //         NewcomerWelcomeMessage: Message option
        //         ReturnedWelcomeMessage: Message option
        //     |}
        Exit:
            {|
                Channel: ChannelId option
                GoodbyeMessage: Message option
            |}
    }

type Response = //Result<unit, Settings>
    | Set of Result<unit, string>
    | Get of Settings

let requestHandler (state: State) (r: Request): option<_> * Response =
    match r with
    | Request.Set (guildId, x) ->
        match x with
        | Checkpoint x ->
            match x with
            // | Checkpoint.DoorkeeperRole r -> failwith "Not Implemented"
            // | Checkpoint.EnteredUserRole r -> failwith "Not Implemented"

            | Checkpoint.Channel channelId ->
                let state =
                    state.WelcomeSetting
                    |> WelcomeSetting.GuildWelcomeSetting.setWelcomeSetting guildId (fun x ->
                        { x with OutputChannel = Some channelId }
                    )

                Some state, Ok ()

            | Checkpoint.NewcomerWelcomeMessage msg ->
                // TODO: check if the message is correct

                let state =
                    state.WelcomeSetting
                    |> WelcomeSetting.GuildWelcomeSetting.setWelcomeSetting guildId (fun x ->
                        { x with TemplateMessage = Some msg }
                    )

                Some state, Ok ()

            | Checkpoint.ReturnedWelcomeMessage msg ->
                // TODO: check if the message is correct

                match WelcomeSetting.GuildWelcomeSetting.tryFind guildId state.WelcomeSetting with
                | Some welcomeSetting ->
                    let channelId =
                        welcomeSetting.OutputChannel
                        |> Option.orElseWith (fun () ->
                            welcomeSetting.LeaversChannelMessage
                            |> Option.map (fun x -> x.ChannelId)
                        )

                    match channelId with
                    | Some channelId ->
                        let state =
                            state.WelcomeSetting
                            |> WelcomeSetting.GuildWelcomeSetting.setWelcomeSetting guildId (fun x ->
                                { x with
                                    LeaversChannelMessage =
                                        WelcomeSetting.ChannelMessage.Init channelId msg
                                        |> Some
                                }
                            )

                        Some state, Ok ()

                    | None ->
                        None, Error "Checkpoint.Channel is None"
                | None ->
                    None, Error "Checkpoint.Channel is None"

        | Exit x ->
            match x with
            | Exit.Channel channelId ->
                let state =
                    state.WelcomeSetting
                    |> WelcomeSetting.GuildWelcomeSetting.setWelcomeSetting guildId (fun x ->
                        { x with OutputLeaveChannel = Some channelId }
                    )

                Some state, Ok ()
            | Exit.GoodbyeMessage msg ->
                // TODO: check if the message is correct

                let state =
                    state.WelcomeSetting
                    |> WelcomeSetting.GuildWelcomeSetting.setWelcomeSetting guildId (fun x ->
                        { x with TemplateLeaveMessage = Some msg }
                    )

                Some state, Ok ()

        |> fun (state, x) -> state, Response.Set x

    | Request.Get guildId ->
        let welcomeSetting = WelcomeSetting.GuildWelcomeSetting.tryFind guildId state.WelcomeSetting

        {
            Checkpoint =
                {|
                    Channel = welcomeSetting |> Option.bind (fun x -> x.OutputChannel)
                    // DoorkeeperRole = RoleId
                    // EnteredUserRole = RoleId
                    NewcomerWelcomeMessage = welcomeSetting |> Option.bind (fun x -> x.TemplateMessage)
                    ReturnedWelcomeMessage =
                        welcomeSetting
                        |> Option.bind (fun x ->
                            x.LeaversChannelMessage |> Option.map (fun x -> x.Message))
                |}
            // Inner =
            //     {|
            //         Channel = ChannelId
            //         NewcomerWelcomeMessage = MessageTemplate
            //         ReturnedWelcomeMessage = MessageTemplate
            //     |}
            Exit =
                {|
                    Channel = welcomeSetting |> Option.bind (fun x -> x.OutputLeaveChannel)
                    GoodbyeMessage = welcomeSetting |> Option.bind (fun x -> x.TemplateLeaveMessage)
                |}
        }
        |> fun res -> None, Response.Get res
