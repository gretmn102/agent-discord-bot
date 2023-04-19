module ImageChannel.Main
open DSharpPlus
open FsharpMyExtension
open FsharpMyExtension.Either

open Types
open Extensions

type SlashCommand =
    | AddChannel of ChannelId
    | RemoveChannel of ChannelId
    | GetChannels

type Msg =
    | RequestSlashCommand of EventArgs.InteractionCreateEventArgs * SlashCommand
    | MessageCreateHandle of EventArgs.MessageCreateEventArgs

type State =
    {
        GuildSettingsDb: Model.GuildSettingsDb.GuildData
    }

let reduce msg (state: State) =
    let intrepGuildSettings guildId req state =
        let req, newMarriedCouples =
            Model.GuildSettingsDb.interp guildId req state.GuildSettingsDb

        let state =
            { state with
                GuildSettingsDb = newMarriedCouples
            }

        req, state

    let interp guildId (author: Entities.DiscordMember) response responseEphemeral getMemberAsync cmd state =
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
                let req, state = intrepGuildSettings guildId req state
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

            | Model.AuthorIsAdmin((), next) ->
                let isAdmin = author.Permissions &&& Permissions.Administrator = Permissions.Administrator
                let req = next isAdmin
                interp req state

            | Model.End -> state

        interp cmd state

    match msg with
    | RequestSlashCommand(e, act) ->
        let user = e.Interaction.User
        let guild = e.Interaction.Guild

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
                guild.GetMemberAsync userId

            interp
                guild.Id
                (getGuildMember guild user)
                response
                responseEphemeral
                getMemberAsync

        match act with
        | AddChannel channelId ->
            interp (Model.addChannel channelId) state
        | RemoveChannel channelId ->
            interp (Model.removeChannel channelId) state
        | GetChannels ->
            interp Model.getChannels state

    | MessageCreateHandle e ->
        let userId = e.Author.Id
        let guildId = e.Guild.Id

        let rec interp req state =
            match req with
            | Model.MessageCreateHandleCmd.SettingsReq req ->
                let req, state = intrepGuildSettings guildId req state
                interp req state

            | Model.MessageCreateHandleCmd.AuthorIsBot((), next) ->
                let req =
                    next e.Author.IsBot

                interp req state

            | Model.MessageCreateHandleCmd.MessageHasAttachment((), next) ->
                let req =
                    next (e.Message.Attachments.Count > 0)
                interp req state

            | Model.MessageCreateHandleCmd.RemoveMessage((), next) ->
                let res =
                    try
                        awaiti <| e.Message.DeleteAsync()
                        Ok ()
                    with e ->
                        Error e.Message

                interp (next res) state
            | Model.MessageCreateHandleCmd.End -> state

        interp (Model.messageCreateHandle e.Channel.Id userId e.Message.Id) state

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

                let removeName = "remove"
                let removeOptName = "target-channel"

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
                                    removeName,
                                    "remove channel",
                                    ApplicationCommandOptionType.SubCommand,
                                    options = [|
                                        Entities.DiscordApplicationCommandOption(
                                            removeOptName,
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

                                elif data.Name = removeName then
                                    let channelId =
                                        data.Options
                                        |> Seq.tryPick (fun x ->
                                            if x.Name = removeOptName then
                                                let v = x.Value :?> ChannelId
                                                Some v
                                            else
                                                None
                                        )
                                    match channelId with
                                    | Some channelId ->
                                        reducer.Post(RequestSlashCommand (e, RemoveChannel channelId))
                                    | None ->
                                        failwithf "not found `%A` in %A" removeOptName (List.ofSeq data.Options)

                                else
                                    failwithf "%A not implemented yet" data.Name
                            )

                            true
                        else
                            false
                |}

            let help =
                {|
                    Command =
                        Entities.DiscordApplicationCommandOption(
                            "help",
                            "help",
                            ApplicationCommandOptionType.SubCommand
                        )

                    Handler = fun (e: EventArgs.InteractionCreateEventArgs) (data: Entities.DiscordInteractionDataOption) ->
                        let embed = new Entities.DiscordEmbedBuilder()
                        embed.Title <- "Каналы с изображениями"
                        embed.Color <- DiscordEmbed.backgroundColorDarkTheme
                        embed.Description <-
                            [
                                "Модуль, который удаляет в указанных каналах сообщения, не содержащие изображения. Это нужно для того, чтобы пользователи создавали ветки под интересующими их изображениями и оставляли комментарии."
                                ""
                                "• </image-channel channels add:1097782673407221851> — добавить канал с изображениями"
                                "• </image-channel channels remove:1097782673407221851>  — удалить канал с изображениями"
                                "• </image-channel channels get-list:1097782673407221851> — просмотреть список добавленных каналов"
                            ]
                            |> String.concat "\n"
                        let b = new Entities.DiscordInteractionResponseBuilder()
                        b.AddEmbed embed |> ignore
                        let typ =
                            InteractionResponseType.ChannelMessageWithSource
                        awaiti <| e.Interaction.CreateResponseAsync(typ, b)

                        true
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
                            help.Command
                        |],
                        ``type`` = ApplicationCommandType.SlashCommand
                    )

                Handler = fun e ->
                    e.Interaction.Data.Options
                    |> Seq.iter (fun data ->
                        let isHandled =
                            channels.Handler e data
                            || help.Handler e data

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

        MessageCreateEventHandle =
            let handle (client, (e: EventArgs.MessageCreateEventArgs)) =
                reducer.Post (MessageCreateHandle e)
            Some handle
    }
