module ImageChannel.Main
open DSharpPlus
open FsharpMyExtension
open FsharpMyExtension.Either

open Types
open Extensions

type SlashCommand =
    | AddChannel of ChannelId
    | GetChannels

type Msg =
    | RequestSlashCommand of EventArgs.InteractionCreateEventArgs * SlashCommand

type State =
    {
        GuildSettingsDb: Model.GuildSettingsDb.GuildData
    }

let reduce msg (state: State) =
    let interp guildId response responseEphemeral getMemberAsync cmd state =
        let rec interp cmd state =
            match cmd with
            | Model.Print(args, next) ->
                let response =
                    if args.IsEphemeral then
                        responseEphemeral
                    else
                        response

                View.view args.Description
                |> response

                interp (next ()) state

            | Model.MarriedCouplesCm req ->
                let req, newMarriedCouples =
                    Model.GuildSettingsDb.interp guildId req state.GuildSettingsDb

                let state =
                    { state with
                        GuildSettingsDb = newMarriedCouples
                    }
                interp req state

            | Model.CreateChannelsView(channels, next) ->
                View.channelsView channels
                |> responseEphemeral

                interp (next ()) state

            | Model.UserIsBot(userId, userIdBot) ->
                let user =
                    try
                        let guildMember: Entities.DiscordMember = await <| getMemberAsync userId
                        Ok guildMember
                    with e ->
                        Error e.Message

                match user with
                | Ok user ->
                    let req = userIdBot user.IsBot

                    interp req state
                | Error(errorValue) ->
                    let b = Entities.DiscordMessageBuilder()
                    b.Content <- sprintf "```\n%s\n```" errorValue
                    responseEphemeral b

                    state

            | Model.End -> state

        interp cmd state

    match msg with
    | RequestSlashCommand(e, act) ->
        let userId = e.Interaction.User.Id
        let guildId = e.Interaction.Guild.Id

        let interp =
            let response (b: Entities.DiscordMessageBuilder) =
                let b = Entities.DiscordInteractionResponseBuilder(b)
                b.AddMentions(Entities.Mentions.All) |> ignore
                let typ =
                    InteractionResponseType.ChannelMessageWithSource
                awaiti <| e.Interaction.CreateResponseAsync (typ, b)

            let responseEphemeral (b: Entities.DiscordMessageBuilder) =
                let b = Entities.DiscordInteractionResponseBuilder(b)
                b.IsEphemeral <- true
                let typ =
                    InteractionResponseType.ChannelMessageWithSource
                awaiti <| e.Interaction.CreateResponseAsync (typ, b)

            let getMemberAsync userId =
                e.Interaction.Guild.GetMemberAsync userId

            interp guildId response responseEphemeral getMemberAsync

        match act with
        | AddChannel channelId ->
            interp (Model.addChannel channelId) state
        | GetChannels ->
            interp Model.getChannels state

let create db =
    let reducer =
        let init: State = {
            GuildSettingsDb = Model.GuildSettingsDb.GuildData.init "guildImageChannelSettings" db
        }

        MailboxProcessor.Start (fun mail ->
            let rec loop (state: State) =
                async {
                    let! msg = mail.Receive()
                    let state =
                        try
                            reduce msg state
                        with e ->
                            printfn "%A" e
                            state

                    return! loop state
                }
            loop init
        )

    let commands =
        let settings =
            let channels =
                let commandName = "channels"
                let addName = "add"
                let addOptName = "target-channel"

                let getName = "get-list"

                {|
                    Command =
                        Entities.DiscordApplicationCommandOption(
                            commandName,
                            "manage channels",
                            ApplicationCommandOptionType.SubCommandGroup,
                            options = [|
                                Entities.DiscordApplicationCommandOption(
                                    addName,
                                    "add channel",
                                    ApplicationCommandOptionType.SubCommand,
                                    options = [|
                                        Entities.DiscordApplicationCommandOption(
                                            addOptName,
                                            "channel",
                                            ApplicationCommandOptionType.Channel,
                                            required = true
                                        )
                                    |]
                                )
                                Entities.DiscordApplicationCommandOption(
                                    getName,
                                    "get all channels",
                                    ApplicationCommandOptionType.SubCommand
                                )
                            |]
                        )

                    Handler = fun (e: EventArgs.InteractionCreateEventArgs) (data: Entities.DiscordInteractionDataOption) ->
                        if data.Name = commandName then
                            data.Options
                            |> Seq.iter (fun data ->
                                if data.Name = getName then
                                    reducer.Post(RequestSlashCommand (e, GetChannels))

                                elif data.Name = addName then
                                    let channelId =
                                        data.Options
                                        |> Seq.tryPick (fun x ->
                                            if x.Name = addOptName then
                                                let v = x.Value :?> ChannelId
                                                Some v
                                            else
                                                None
                                        )
                                    match channelId with
                                    | Some channelId ->
                                        reducer.Post(RequestSlashCommand (e, AddChannel channelId))
                                    | None ->
                                        failwithf "not found `%A` in %A" addOptName (List.ofSeq data.Options)

                                else
                                    failwithf "%A not implemented yet" data.Name
                            )

                            true
                        else
                            false
                |}

            let slashCommandName = "image-channel"

            InteractionCommand.SlashCommand {|
                CommandName = slashCommandName
                Command =
                    new Entities.DiscordApplicationCommand(
                        slashCommandName,
                        "Managing ImageChannel settings",
                        [|
                            channels.Command
                            // messageTemplates.Command
                        |],
                        ``type`` = ApplicationCommandType.SlashCommand
                    )

                Handler = fun e ->
                    e.Interaction.Data.Options
                    |> Seq.iter (fun data ->
                        let isHandled =
                            channels.Handler e data
                            // || messageTemplates.Handler e data

                        if not isHandled then
                            failwithf "%A not implemented yet" data.Name
                    )
            |}

        [|
            settings
        |]

    { Shared.BotModule.empty with
        InteractionCommands =
            Some commands
    }
