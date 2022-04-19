/// An entity that welcomes newbies and give a role or says goodbye to the leavers
module Doorkeeper.Main
open FsharpMyExtension
open FsharpMyExtension.Either
open DSharpPlus

open Types
open Model

type Template =
    | Text of string
    | UserName
    | UserMention
    static member UserNameName = "userName"
    static member UserMentionName = "userMention"

    static member ToString = function
        | Text x -> x
        | UserName -> sprintf "<@%s>" Template.UserNameName
        | UserMention -> sprintf "<@%s>" Template.UserMentionName

type PassMsg =
    | Pass of UserId * string list
    | SetPassSetting of Either<string, NewcomersRoles.PassSettings>
    | GetPassSetting

type NewcomersRolesMsg =
    | SetNewcomersRoles of RoleId list
    | GetNewcomersRoles
    | PassMsg of PassMsg

type WelcomeSettingMsg =
    | SetWelcomeSetting of ChannelId * Template list
    | SetWelcomeLogSetting of ChannelId * Template list
    | SetWelcomeLeaverMessage of ChannelId * Template list
    | SetWelcomeLeaverLogMessage of ChannelId * Template list
    | SetWelcomeLeaveSetting of ChannelId * Template list

type Request =
    | NewcomersRolesReq of NewcomersRolesMsg
    | WelcomeSettingReq of WelcomeSettingMsg

module Parser =
    open FParsec

    open DiscordMessage.Parser

    type 'Result Parser = Primitives.Parser<'Result, unit>

    let pgetNewcomersRoles: _ Parser =
        skipStringCI "newcomersRoles"

    let psetNewcomersRoles: RoleId list Parser =
        skipStringCI "setNewcomersRoles" >>. spaces
        >>. many (pmentionRole <|> puint64 .>> spaces)

    let ptemplateMessage: _ Parser =
        let praw = many1Satisfy ((<>) '<')
        let pall =
            choice [
                praw |>> Template.Text
                puserMentionTargetStr Template.UserNameName >>% UserName
                puserMentionTargetStr Template.UserMentionName >>% UserMention
                pstring "<" |>> Template.Text
            ]

        many pall

    let pchannelTemplateMessage cmdName =
        skipStringCI cmdName >>. spaces
        >>. tuple2
                (pchannelMention .>> spaces)
                ptemplateMessage

    let start: _ Parser =
        let ppassCommand =
            let ppass: _ Parser =
                skipStringCI "pass" >>. spaces
                >>. tuple2
                        (puserMention .>> spaces)
                        (many (many1Satisfy (fun _ -> true)))

            let psetPassSetting: _ Parser =
                skipStringCI "setPassSettings" >>. spaces
                >>. (pcodeBlock <|> manySatisfy (fun _ -> true))
                |>> (fun str ->
                    try
                        let passSettings: NewcomersRoles.PassSettings = Json.des str
                        Right(passSettings)
                    with e ->
                        Left e.Message
                )

            choice [
                psetPassSetting |>> SetPassSetting
                skipStringCI "getPassSettings" >>% GetPassSetting
                ppass |>> Pass
            ]

        let p =
            choice [
                psetNewcomersRoles |>> SetNewcomersRoles
                pgetNewcomersRoles >>% GetNewcomersRoles
                ppassCommand |>> PassMsg
            ]
        let psettings =
            choice [
                pchannelTemplateMessage "setWelcomeSetting" |>> SetWelcomeSetting
                pchannelTemplateMessage "setWelcomeLogSetting" |>> SetWelcomeLogSetting
                pchannelTemplateMessage "setWelcomeLeaverMessage" |>> SetWelcomeLeaverMessage
                pchannelTemplateMessage "setWelcomeLeaverLogMessage" |>> SetWelcomeLeaverLogMessage
                pchannelTemplateMessage "setWelcomeLeaveSetting" |>> SetWelcomeLeaveSetting
            ]

        choice [
            p |>> NewcomersRolesReq
            psettings |>> WelcomeSettingReq
        ]

let templateBuild (targetUser: Entities.DiscordMember) message =
    FParsecUtils.runEither Parser.ptemplateMessage message
    |> Either.map (
        List.map (function
            | Text x -> x
            | UserMention -> targetUser.Mention
            | UserName -> targetUser.Username
        )
        >> System.String.Concat
    )

let templateToString template =
    template |> List.map Template.ToString |> System.String.Concat

type Msg =
    | Request of EventArgs.MessageCreateEventArgs * Request
    | GuildMemberAddedHandle of EventArgs.GuildMemberAddEventArgs
    | GuildMemberRemovedHandle of EventArgs.GuildMemberRemoveEventArgs

type State =
    {
        NewcomersRoles: NewcomersRoles.GuildNewcomersRoles
        WelcomeSetting: WelcomeSetting.GuildWelcomeSetting
        Leavers: Leavers.GuildDatas
    }

let newcomersRolesReduce
    (e: EventArgs.MessageCreateEventArgs)
    (msg: NewcomersRolesMsg)
    (guildNewcomersRoles: NewcomersRoles.GuildNewcomersRoles): NewcomersRoles.GuildNewcomersRoles =

    let passErrorMsg =
        [
            "This server doesn't yet have pass settings. To set them, use the command:"
            "```"
            ".setPassSettings"
            NewcomersRoles.PassSettings.SampleJson
            "```"
        ] |> String.concat "\n"

    match msg with
    | PassMsg msg ->
        match msg with
        | Pass (targetUserId, roleKeys) ->
            let guild = e.Guild
            let currentMember = getGuildMember guild e.Author
            let replyMessage =
                await (e.Channel.SendMessageAsync("Processing..."))

            let sendErrorMessage () =
                awaiti (replyMessage.ModifyAsync (Entities.Optional passErrorMsg))

            match Map.tryFind e.Guild.Id guildNewcomersRoles with
            | Some newcomersRoles ->
                match newcomersRoles.PassSettings with
                | Some passSetting ->
                    let permittedRoles = passSetting.PermittedRoles
                    let isAllowed =
                        currentMember.Roles
                        |> Seq.exists (fun x ->
                            Set.contains x.Id permittedRoles)
                    if isAllowed then
                        let roleKeys =
                            let rec f acc = function
                                | x::xs ->
                                    let res =
                                        passSetting.IssuedRoleIds
                                        |> Array.tryFind (fun (key', _) -> key' = x)
                                    match res with
                                    | Some roleId -> f (roleId::acc) xs
                                    | None -> Left x
                                | [] -> Right acc
                            f [] roleKeys

                        match roleKeys with
                        | Left notFoundKey ->
                            let embed = Entities.DiscordEmbedBuilder()
                            embed.Color <- Entities.Optional.FromValue(Entities.DiscordColor("#2f3136"))
                            embed.Description <-
                                sprintf
                                    "Not found \"%s\" key. Available next keys:\n%s"
                                    notFoundKey
                                    (passSetting.IssuedRoleIds
                                     |> Seq.map (fun (k, roleId) ->
                                        sprintf "* %s <@&%d>" k roleId)
                                     |> String.concat "\n")

                            let b = Entities.DiscordMessageBuilder()
                            b.Embed <- embed.Build()

                            awaiti (replyMessage.ModifyAsync b)
                        | Right roleKeys ->
                            match await (guild.GetMemberAsync targetUserId) with
                            | null ->
                                let msg =
                                    sprintf "User <@!%d> is not a member of this guild" targetUserId

                                awaiti (replyMessage.ModifyAsync (Entities.Optional msg))
                            | targetUser ->
                                newcomersRoles.RoleIds
                                |> Set.iter (fun roleId ->
                                    match guild.GetRole roleId with
                                    | null -> ()
                                    | role ->
                                        try
                                            targetUser.RevokeRoleAsync(role).GetAwaiter().GetResult()
                                        with e ->
                                            printfn "%A" e
                                )

                                roleKeys
                                |> List.iter (fun (_, roleId) ->
                                    match guild.GetRole roleId with
                                    | null -> ()
                                    | role ->
                                        try
                                            targetUser.GrantRoleAsync(role).GetAwaiter().GetResult()
                                        with e ->
                                            printfn "%A" e
                                )

                                // send welcome message to main channel
                                match guild.GetChannel passSetting.MainChannelId with
                                | null ->
                                    let msg =
                                        sprintf "Channel %d not found" passSetting.MainChannelId

                                    awaiti (replyMessage.ModifyAsync (Entities.Optional msg))

                                | mainChannel ->
                                    FParsecUtils.runEither Parser.ptemplateMessage passSetting.WelcomeMessage
                                    |> Either.map (
                                        List.map (function
                                            | Text x -> x
                                            | UserMention -> targetUser.Mention
                                            | UserName -> targetUser.Username
                                        )
                                        >> System.String.Concat
                                    )
                                    |> Either.iter (fun msg ->
                                        awaiti <| mainChannel.SendMessageAsync msg
                                    )

                                match passSetting.PassLogMessage with
                                | Some(channelId, message) ->
                                    match guild.GetChannel channelId with
                                    | null -> ()
                                    | channel ->
                                        FParsecUtils.runEither Parser.ptemplateMessage message
                                        |> Either.map (
                                            List.map (function
                                                | Text x -> x
                                                | UserMention -> targetUser.Mention
                                                | UserName -> targetUser.Username
                                            )
                                            >> System.String.Concat
                                        )
                                        |> Either.iter (fun msg ->
                                            try
                                                awaiti <| channel.SendMessageAsync msg
                                            with _ -> ()
                                        )

                                | None -> ()

                                awaiti (replyMessage.ModifyAsync (Entities.Optional "Done"))
                    else
                        let embed = Entities.DiscordEmbedBuilder()
                        embed.Color <- Entities.Optional.FromValue(Entities.DiscordColor("#2f3136"))
                        embed.Description <-
                            sprintf
                                "This command is only allowed for users who have these roles: %s"
                                (permittedRoles |> Seq.map (sprintf "<@&%d>") |> String.concat ", ")

                        let b = Entities.DiscordMessageBuilder()
                        b.Embed <- embed.Build()

                        // let msg =
                        //     Entities.Optional (embed.Build())
                        // awaiti (replyMessage.ModifyAsync (Entities.Optional.FromNoValue<string>(), msg))
                        awaiti (replyMessage.ModifyAsync b)
                | None ->
                    sendErrorMessage ()

            | None ->
                sendErrorMessage ()

            guildNewcomersRoles
        | SetPassSetting passSettings ->
            let guild = e.Guild
            let currentMember = getGuildMember guild e.Author
            let replyMessage =
                await (e.Channel.SendMessageAsync("Processing..."))

            match passSettings with
            | Right passSettings ->
                if (currentMember.Permissions &&& Permissions.Administrator = Permissions.Administrator) then
                    let guildNewcomersRoles =
                        match Map.tryFind e.Guild.Id guildNewcomersRoles with
                        | Some newcomersRoles ->

                            let newcomersRoles =
                                { newcomersRoles with
                                    PassSettings = Some passSettings }

                            NewcomersRoles.replace newcomersRoles

                            Map.add guild.Id newcomersRoles guildNewcomersRoles
                        | None ->
                            let x = NewcomersRoles.insert (guild.Id, Set.empty, Some passSettings)
                            Map.add guild.Id x guildNewcomersRoles

                    awaiti (replyMessage.ModifyAsync(Entities.Optional("Pass settings has been set")))

                    guildNewcomersRoles
                else
                    awaiti (replyMessage.ModifyAsync(Entities.Optional("You don't have permission to manage roles")))

                    guildNewcomersRoles
            | Left errMsg ->
                awaiti (replyMessage.ModifyAsync(Entities.Optional errMsg))

                guildNewcomersRoles

        | GetPassSetting ->
            let message =
                let sendErrorMessage () =
                    let b = Entities.DiscordMessageBuilder()
                    let embed = Entities.DiscordEmbedBuilder()
                    embed.Description <- passErrorMsg
                    b.Embed <- embed.Build()
                    b

                match Map.tryFind e.Guild.Id guildNewcomersRoles with
                | Some newcomersRoles->
                    match newcomersRoles.PassSettings with
                    | Some passSettings ->
                        let b = Entities.DiscordMessageBuilder()
                        let embed = Entities.DiscordEmbedBuilder()
                        embed.Color <- Entities.Optional.FromValue(Entities.DiscordColor("#2f3136"))
                        embed.Description <-
                            [
                                "```"
                                Json.ser passSettings
                                "```"
                            ] |> String.concat "\n"

                        b.Embed <- embed.Build()
                        b
                    | None ->
                        sendErrorMessage ()
                | None ->
                    sendErrorMessage ()

            awaiti (e.Channel.SendMessageAsync (message))

            guildNewcomersRoles

    | SetNewcomersRoles roleIds ->
        let guild = e.Guild
        let currentMember = getGuildMember guild e.Author
        let replyMessage =
            await (e.Channel.SendMessageAsync("Processing..."))

        if (currentMember.Permissions &&& Permissions.Administrator = Permissions.Administrator)
           || (currentMember.Permissions &&& Permissions.ManageRoles = Permissions.ManageRoles) then
            let guildNewcomersRoles: NewcomersRoles.GuildNewcomersRoles =
                match Map.tryFind e.Guild.Id guildNewcomersRoles with
                | Some newcomersRoles ->
                    let newcomersRoles =
                        { newcomersRoles with
                            RoleIds = Set.ofList roleIds }

                    NewcomersRoles.replace newcomersRoles

                    Map.add guild.Id newcomersRoles guildNewcomersRoles
                | None ->
                    let x = NewcomersRoles.insert (guild.Id, Set.ofList roleIds, None)
                    Map.add guild.Id x guildNewcomersRoles

            awaiti (replyMessage.ModifyAsync(Entities.Optional("Roles has been set")))

            guildNewcomersRoles
        else
            awaiti (replyMessage.ModifyAsync(Entities.Optional("You don't have permission to manage roles")))

            guildNewcomersRoles

    | GetNewcomersRoles ->
        let message =
            match Map.tryFind e.Guild.Id guildNewcomersRoles with
            | Some newcomersRoles ->
                let b = Entities.DiscordMessageBuilder()
                let embed = Entities.DiscordEmbedBuilder()
                embed.Color <- Entities.Optional.FromValue(Entities.DiscordColor("#2f3136"))
                embed.Description <-
                    [
                        yield "Newcomers roles:"
                        yield! newcomersRoles.RoleIds |> Set.map (sprintf "* <@&%d>")
                    ] |> String.concat "\n"

                b.Embed <- embed.Build()
                b
            | None ->
                let b = Entities.DiscordMessageBuilder()
                let embed = Entities.DiscordEmbedBuilder()
                embed.Description <-
                    [
                        "This server doesn't yet have newcomers roles. To set them, use the command:"
                        "```"
                        ".setNewcomersRoles <role1_mention|role1_id> <role2_mention|role2_id> ..."
                        "```"
                    ] |> String.concat "\n"
                b.Embed <- embed.Build()
                b

        awaiti (e.Channel.SendMessageAsync (message))

        guildNewcomersRoles

let welcomeSettingReduce
    (e: EventArgs.MessageCreateEventArgs)
    (msg: WelcomeSettingMsg)
    (guildWelcomeSetting: WelcomeSetting.GuildWelcomeSetting): WelcomeSetting.GuildWelcomeSetting =

    match msg with
    | SetWelcomeSetting(channelId, template) ->
        let guild = e.Guild
        let currentMember = getGuildMember guild e.Author
        let replyMessage =
            await (e.Channel.SendMessageAsync("Processing..."))

        if (currentMember.Permissions &&& Permissions.Administrator = Permissions.Administrator) then
            let template = template |> List.map Template.ToString |> String.concat ""
            let guildWelcomeSetting: WelcomeSetting.GuildWelcomeSetting =
                match Map.tryFind e.Guild.Id guildWelcomeSetting with
                | Some welcomeSettingData ->
                    let newcomersRoles =
                        { welcomeSettingData with
                            OutputChannel = Some channelId
                            TemplateMessage =
                                Some template
                        }

                    WelcomeSetting.replace newcomersRoles

                    Map.add guild.Id newcomersRoles guildWelcomeSetting
                | None ->
                    let x = WelcomeSetting.insert (guild.Id, Some channelId, Some template, None, None, None, None, None, None)
                    Map.add guild.Id x guildWelcomeSetting

            awaiti (replyMessage.ModifyAsync(Entities.Optional("Welcome setting has been set")))

            guildWelcomeSetting
        else
            awaiti (replyMessage.ModifyAsync(Entities.Optional("You don't have permission to manage roles")))

            guildWelcomeSetting

    | SetWelcomeLogSetting(channelId, template) ->
        let guild = e.Guild
        let currentMember = getGuildMember guild e.Author
        let replyMessage =
            await (e.Channel.SendMessageAsync("Processing..."))

        if (currentMember.Permissions &&& Permissions.Administrator = Permissions.Administrator) then
            let template = template |> List.map Template.ToString |> String.concat ""
            let guildWelcomeSetting: WelcomeSetting.GuildWelcomeSetting =
                match Map.tryFind e.Guild.Id guildWelcomeSetting with
                | Some welcomeSettingData ->
                    let newcomersRoles =
                        { welcomeSettingData with
                            OutputLogChannel = Some channelId
                            TemplateLogMessage =
                                Some template
                        }

                    WelcomeSetting.replace newcomersRoles

                    Map.add guild.Id newcomersRoles guildWelcomeSetting
                | None ->
                    let x = WelcomeSetting.insert (guild.Id, None, None, Some channelId, Some template, None, None, None, None)
                    Map.add guild.Id x guildWelcomeSetting

            awaiti (replyMessage.ModifyAsync(Entities.Optional("Welcome log setting has been set")))

            guildWelcomeSetting
        else
            awaiti (replyMessage.ModifyAsync(Entities.Optional("You don't have permission to manage roles")))

            guildWelcomeSetting

    | SetWelcomeLeaveSetting(channelId, template) ->
        let guild = e.Guild
        let currentMember = getGuildMember guild e.Author
        let replyMessage =
            await (e.Channel.SendMessageAsync("Processing..."))

        if (currentMember.Permissions &&& Permissions.Administrator = Permissions.Administrator) then
            let template = template |> List.map Template.ToString |> String.concat ""
            let guildWelcomeSetting: WelcomeSetting.GuildWelcomeSetting =
                match Map.tryFind e.Guild.Id guildWelcomeSetting with
                | Some welcomeSettingData ->
                    let newcomersRoles =
                        { welcomeSettingData with
                            OutputLeaveChannel = Some channelId
                            TemplateLeaveMessage =
                                Some template
                        }

                    WelcomeSetting.replace newcomersRoles

                    Map.add guild.Id newcomersRoles guildWelcomeSetting
                | None ->
                    let x = WelcomeSetting.insert (guild.Id, None, None, None, None, Some channelId, Some template, None, None)
                    Map.add guild.Id x guildWelcomeSetting

            awaiti (replyMessage.ModifyAsync(Entities.Optional("Welcome leave setting has been set")))

            guildWelcomeSetting
        else
            awaiti (replyMessage.ModifyAsync(Entities.Optional("You don't have permission to manage roles")))

            guildWelcomeSetting
    | SetWelcomeLeaverMessage(channelId, template) ->
        let guild = e.Guild
        let currentMember = getGuildMember guild e.Author
        let replyMessage =
            await (e.Channel.SendMessageAsync("Processing..."))

        if (currentMember.Permissions &&& Permissions.Administrator = Permissions.Administrator) then
            let template = template |> List.map Template.ToString |> String.concat ""
            let guildWelcomeSetting: WelcomeSetting.GuildWelcomeSetting =
                match Map.tryFind e.Guild.Id guildWelcomeSetting with
                | Some welcomeSettingData ->
                    let newcomersRoles =
                        { welcomeSettingData with
                            LeaversChannelMessage =
                                WelcomeSetting.ChannelMessage.Init channelId template
                                |> Some
                        }

                    WelcomeSetting.replace newcomersRoles

                    Map.add guild.Id newcomersRoles guildWelcomeSetting
                | None ->
                    let channelMessage = WelcomeSetting.ChannelMessage.Init channelId template
                    let x = WelcomeSetting.insert (guild.Id, None, None, None, None, None, None, Some channelMessage, None)
                    Map.add guild.Id x guildWelcomeSetting

            awaiti (replyMessage.ModifyAsync(Entities.Optional("Welcome leaver setting has been set")))

            guildWelcomeSetting
        else
            awaiti (replyMessage.ModifyAsync(Entities.Optional("You don't have permission to manage roles")))

            guildWelcomeSetting
    | SetWelcomeLeaverLogMessage(channelId, template) ->
        let guild = e.Guild
        let currentMember = getGuildMember guild e.Author
        let replyMessage =
            await (e.Channel.SendMessageAsync("Processing..."))

        if (currentMember.Permissions &&& Permissions.Administrator = Permissions.Administrator) then
            let template = template |> List.map Template.ToString |> String.concat ""
            let guildWelcomeSetting: WelcomeSetting.GuildWelcomeSetting =
                match Map.tryFind e.Guild.Id guildWelcomeSetting with
                | Some welcomeSettingData ->
                    let newcomersRoles =
                        { welcomeSettingData with
                            LeaversLogChannelMessage =
                                WelcomeSetting.ChannelMessage.Init channelId template
                                |> Some
                        }

                    WelcomeSetting.replace newcomersRoles

                    Map.add guild.Id newcomersRoles guildWelcomeSetting
                | None ->
                    let channelMessage = WelcomeSetting.ChannelMessage.Init channelId template
                    let x = WelcomeSetting.insert (guild.Id, None, None, None, None, None, None, None, Some channelMessage)
                    Map.add guild.Id x guildWelcomeSetting

            awaiti (replyMessage.ModifyAsync(Entities.Optional("Welcome leaver log setting has been set")))

            guildWelcomeSetting
        else
            awaiti (replyMessage.ModifyAsync(Entities.Optional("You don't have permission to manage roles")))

            guildWelcomeSetting

let reduce (msg: Msg) (state: State): State =
    match msg with
    | GuildMemberAddedHandle e ->
        if not e.Member.IsBot then
            let guildId = e.Guild.Id

            let grantRoles roles =
                roles
                |> Seq.iter (fun roleId ->
                    match e.Guild.Roles.[roleId] with
                    | null -> ()
                    | role ->
                        try
                            e.Member.GrantRoleAsync(role)
                            |> fun x -> x.GetAwaiter() |> fun x -> x.GetResult()
                        with e ->
                            printfn "%A" e.Message
                )

            match Map.tryFind guildId state.NewcomersRoles with
            | Some data ->
                grantRoles data.RoleIds
            | None -> ()

            match Map.tryFind guildId state.WelcomeSetting with
            | Some data ->
                let send (channelMessage: WelcomeSetting.ChannelMessage) =
                    match e.Guild.GetChannel channelMessage.ChannelId with
                    | null -> ()
                    | outputChannel ->
                        FParsecUtils.runEither Parser.ptemplateMessage channelMessage.Message
                        |> Either.map (
                            List.map (function
                                | Text x -> x
                                | UserMention -> e.Member.Mention
                                | UserName -> e.Member.Username
                            )
                            >> System.String.Concat
                        )
                        |> Either.iter (fun msg ->
                            try
                                awaiti <| outputChannel.SendMessageAsync msg
                            with e ->
                                printfn "%A" e
                        )

                let sendOption = function
                    | Some outputChannelId, Some templateMessage ->
                        send (WelcomeSetting.ChannelMessage.Init outputChannelId templateMessage)
                    | _ -> ()

                match Map.tryFind guildId state.Leavers with
                | Some users ->
                    match Map.tryFind e.Member.Id users with
                    | Some user ->
                        data.LeaversChannelMessage |> Option.iter send
                        data.LeaversLogChannelMessage |> Option.iter send

                        grantRoles user.RoleIds

                        { state with
                            Leavers =
                                Leavers.remove user
                                let datas = Map.remove e.Member.Id users
                                Map.add guildId datas state.Leavers
                        }
                    | None ->
                        sendOption (data.OutputChannel, data.TemplateMessage)
                        sendOption (data.OutputLogChannel, data.TemplateLogMessage)

                        state
                | None ->
                    sendOption (data.OutputChannel, data.TemplateMessage)
                    sendOption (data.OutputLogChannel, data.TemplateLogMessage)

                    state

            | None ->
                state

        else
            state

    | GuildMemberRemovedHandle e ->
        if not e.Member.IsBot then
            let guildId = e.Guild.Id

            match Map.tryFind guildId state.WelcomeSetting with
            | Some data ->
                let send = function
                    | Some outputChannelId, Some templateMessage ->
                        match e.Guild.GetChannel outputChannelId with
                        | null -> ()
                        | outputChannel ->
                            FParsecUtils.runEither Parser.ptemplateMessage templateMessage
                            |> Either.map (
                                List.map (function
                                    | Text x -> x
                                    | UserMention -> e.Member.Mention
                                    | UserName -> e.Member.Username
                                )
                                >> System.String.Concat
                            )
                            |> Either.iter (fun msg ->
                                try
                                    awaiti <| outputChannel.SendMessageAsync msg
                                with e ->
                                    printfn "%A" e
                            )
                    | _ -> ()

                send (data.OutputLeaveChannel, data.TemplateLeaveMessage)

                { state with
                    Leavers =
                        let roleIds =
                            e.Member.Roles
                            |> Seq.map (fun role -> role.Id)
                            |> Array.ofSeq

                        state.Leavers
                        |> Map.addOrModWith
                            guildId
                            (fun () ->
                                let data = Leavers.insert (guildId, e.Member.Id, roleIds)

                                Map.add e.Member.Id data Map.empty
                            )
                            (fun guild ->
                                guild
                                |> Map.addOrModWith
                                    e.Member.Id
                                    (fun () ->
                                        Leavers.insert (guildId, e.Member.Id, roleIds)
                                    )
                                    (fun data ->
                                        let data =
                                            { data with
                                                RoleIds = roleIds
                                            }

                                        Leavers.replace data

                                        data
                                    )
                            )
                }
            | None ->
                state
        else
            state

    | Request(e, cmd) ->
        match cmd with
        | NewcomersRolesReq cmd ->
            { state with
                NewcomersRoles =
                    newcomersRolesReduce e cmd state.NewcomersRoles
            }
        | WelcomeSettingReq cmd ->
            { state with
                WelcomeSetting =
                    welcomeSettingReduce e cmd state.WelcomeSetting
            }

let m =
    let init = {
        NewcomersRoles = NewcomersRoles.getAll ()
        WelcomeSetting = WelcomeSetting.getAll ()
        Leavers = Leavers.getAll ()
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

let guildMemberAddHandle (e: EventArgs.GuildMemberAddEventArgs) =
    m.Post (GuildMemberAddedHandle e)

let guildMemberRemoveHandle (e: EventArgs.GuildMemberRemoveEventArgs) =
    m.Post (GuildMemberRemovedHandle e)

let execNewcomersRolesCmd e msg =
    m.Post (Request (e, msg))

