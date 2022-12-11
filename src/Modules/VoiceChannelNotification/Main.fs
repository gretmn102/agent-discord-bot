module VoiceChannelNotification.Main
open FsharpMyExtension
open FsharpMyExtension.Either
open DSharpPlus

open Types
open Model

type VoiceNotificationMsg =
    | SetOutput of ChannelId
    | SetTemplateMsg of string

type Msg =
    | VoiceNotificationMsg of EventArgs.MessageCreateEventArgs * VoiceNotificationMsg
    | VoiceStateUpdatedHandle of EventArgs.VoiceStateUpdateEventArgs

type State =
    {
        VoiceNotifications: VoiceNotification.Guilds
    }

module Parser =
    open FParsec

    open DiscordMessage.Parser

    type 'a Parser = Parser<'a, unit>

    type Template =
        | Text of string
        | UserName
        | NickName
        | VoiceChannel

    let ptemplateMessage: _ Parser =
        let praw = many1Satisfy ((<>) '<')
        let pall =
            choice [
                praw |>> Text
                puserMentionTargetStr "userName" >>% UserName
                puserMentionTargetStr "nickName" >>% NickName
                pchannelMentionTargetStr "voiceChannel" >>% VoiceChannel
                pstring "<" |>> Text
            ]

        many pall

    let psetVoiceNotificationOutput: _ Parser =
        skipStringCI "setVoiceNotificationOutput" >>. spaces
        >>. (pmentionRole <|> puint64 .>> spaces)
        |>> SetOutput

    let psetVoiceNotificationTemplateMsg: _ Parser =
        skipStringCI "setVoiceNotificationTemplateMsg" >>. spaces
        >>. manySatisfy (fun _ -> true)
        |>> SetTemplateMsg

    let start f: _ Parser =
        choice [
            psetVoiceNotificationOutput
            psetVoiceNotificationTemplateMsg
        ]
        >>= fun msg ->
            preturn (fun x -> f x msg)

let voiceNotificationReduce
    (e: EventArgs.MessageCreateEventArgs)
    (msg: VoiceNotificationMsg)
    (guildVoiceNotification: VoiceNotification.Guilds): VoiceNotification.Guilds =

    match msg with
    | SetOutput outputChannelId ->
        let guild = e.Guild
        let currentMember = getGuildMember guild e.Author
        let replyMessage =
            await (e.Channel.SendMessageAsync("Processing..."))

        if (currentMember.Permissions &&& Permissions.Administrator = Permissions.Administrator)
            || (Db.superUserId = e.Author.Id) then

            let guildVoiceNotification =
                guildVoiceNotification
                |> VoiceNotification.Guilds.set
                    e.Guild.Id
                    (fun voiceNotificationData ->
                        { voiceNotificationData with
                            OutputChannelId = Some outputChannelId
                        }
                    )

            awaiti (replyMessage.ModifyAsync(Entities.Optional("Output channel has been set")))

            guildVoiceNotification
        else
            awaiti (replyMessage.ModifyAsync(Entities.Optional("You don't have permission to manage roles")))

            guildVoiceNotification

    | SetTemplateMsg templateMessage ->
        let guild = e.Guild
        let currentMember = getGuildMember guild e.Author
        let replyMessage =
            await (e.Channel.SendMessageAsync("Processing..."))

        if (currentMember.Permissions &&& Permissions.Administrator = Permissions.Administrator)
            || (Db.superUserId = e.Author.Id) then

            match FParsecExt.runEither Parser.ptemplateMessage templateMessage with
            | Right _ ->
                let guildVoiceNotification =
                    guildVoiceNotification
                    |> VoiceNotification.Guilds.set
                        e.Guild.Id
                        (fun voiceNotificationData ->
                            { voiceNotificationData with
                                TemplateMessage = Some templateMessage
                            }
                        )

                awaiti (replyMessage.ModifyAsync(Entities.Optional("Template message has been set")))

                guildVoiceNotification
            | Left err ->
                awaiti (replyMessage.ModifyAsync(Entities.Optional(sprintf "```\n%s\n```" err)))

                guildVoiceNotification
        else
            awaiti (replyMessage.ModifyAsync(Entities.Optional("You don't have permission to manage roles")))

            guildVoiceNotification

let reduce (msg: Msg) (state: State): State =
    match msg with
    | VoiceStateUpdatedHandle e ->
        match VoiceNotification.Guilds.tryFindById e.Guild.Id state.VoiceNotifications with
        | Some settings ->
            match settings.Data.OutputChannelId, settings.Data.TemplateMessage with
            | Some outputChannelId, Some templateMsg ->
                match e.After with
                | null -> state
                | after ->
                    match after.Channel with
                    | null -> state
                    | enteredVoiceChannel ->
                        let isDiff =
                            match e.Before with
                            | null -> true
                            | before ->
                                match before.Channel with
                                | null -> true
                                | beforeChannel ->
                                    beforeChannel.Id <> enteredVoiceChannel.Id

                        if isDiff then
                            let outputChannel = e.Guild.GetChannel(outputChannelId)
                            let guildMember = getGuildMember e.Guild e.User

                            let message =
                                FParsecExt.runEither Parser.ptemplateMessage templateMsg
                                |> Either.map (
                                    List.map (function
                                        | Parser.Text x -> x
                                        | Parser.NickName ->
                                            match guildMember.Nickname with
                                            | null -> e.User.Username
                                            | x -> x
                                        | Parser.UserName -> e.User.Username
                                        | Parser.VoiceChannel ->
                                            sprintf "<#%d>" enteredVoiceChannel.Id
                                    )
                                    >> System.String.Concat
                                )

                            message
                            |> Either.iter (fun message ->
                                awaiti (outputChannel.SendMessageAsync message)
                            )

                            state
                        else
                            state
            | _ -> state
        | None -> state

    | VoiceNotificationMsg(e, msg) ->
        { state with
            VoiceNotifications =
                voiceNotificationReduce e msg state.VoiceNotifications
        }

let create db =
    let m =
        let init = {
            VoiceNotifications = VoiceNotification.Guilds.init "voiceNotifications" db
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

    { Shared.BotModule.empty with
        MessageCreateEventHandleExclude =
            let exec: _ Parser.Parser =
                Parser.start (fun (client: DiscordClient, e: EventArgs.MessageCreateEventArgs) msg ->
                    m.Post(VoiceNotificationMsg (e, msg))
                )
            Some exec

        VoiceStateUpdatedHandler =
            let voiceHandle e =
                m.Post (VoiceStateUpdatedHandle e)
            Some voiceHandle
    }
