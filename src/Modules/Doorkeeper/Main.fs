/// An entity that welcomes newbies and give a role or says goodbye to the leavers
module Doorkeeper.Main
open FsharpMyExtension
open FsharpMyExtension.Either
open DSharpPlus

open Shared
open Types
open Extensions
open Model

module Builder =
    let getEnabledOptionValueSilent enabledOptionValue next =
        let isEnabledDoorkeeperRole next =
            if enabledOptionValue.IsEnabled then
                next ()
            else
                ()

        let getDoorkeeperRole next =
            match enabledOptionValue.Value with
            | Some permittedRole ->
                next permittedRole
            | None ->
                ()

        isEnabledDoorkeeperRole <| fun () ->
        getDoorkeeperRole <| next

    let sendTemplateMessage (targetUser: Entities.DiscordMember) (channel: Entities.DiscordChannel) templateMessage =
        MessageTemplate.Message.parse templateMessage
        |> Either.iter (fun msg ->
            let msg = MessageTemplate.Message.substitute targetUser.Mention targetUser.Username msg
            try
                awaiti <| channel.SendMessageAsync msg
            with _ -> ()
        )

    let getChannelSilent (channelId: ChannelId) (guild: Entities.DiscordGuild) next =
        match guild.GetChannel channelId with
        | null ->
            ()

        | mainChannel ->
            next mainChannel

    let grantRoleSilent (guild: Entities.DiscordGuild) (roleId: RoleId) (targetUser: Entities.DiscordMember) =
        match guild.Roles.[roleId] with
        | null -> ()
        | role ->
            try
                targetUser.GrantRoleAsync(role)
                |> fun x -> x.GetAwaiter() |> fun x -> x.GetResult()
            with e ->
                printfn "%A" e.Message

    let (^<|) f x = f x

    let isUserNotBot (user: Entities.DiscordMember) next state =
        if user.IsBot then
            state
        else
            next () state

    let getSettingSilent (guildId: GuildId) settings next state =
        match Setting.GuildData.tryFind guildId settings with
        | Some newcomersRoles ->
            next newcomersRoles state
        | None ->
            state

    let isUserHasAdministrativeRight sendMessage (currentMember: Entities.DiscordMember) next state =
        if currentMember.Permissions &&& Permissions.Administrator = Permissions.Administrator then
            next () state
        else
            sendMessage "You don't have administration permission"
            state

    let send guild user message channel =
        getEnabledOptionValueSilent message <| fun message ->
            getEnabledOptionValueSilent channel <| fun channelId ->
            getChannelSilent channelId guild <| fun channel ->
            sendTemplateMessage user channel message

    let r = System.Random()
    let sendRandom guild user message channel =
        getEnabledOptionValueSilent message <| fun messages ->
            let len = Array.length messages
            if len > 0 then
                getEnabledOptionValueSilent channel <| fun channelId ->
                getChannelSilent channelId guild <| fun channel ->

                let n = r.Next(0, len)
                let message = messages.[n]

                sendTemplateMessage user channel message

open Builder

type SettingReq =
    | Set of Result<Api.TransferTypes.MainData, string>
    | Get

type ActionReq =
    | Pass of UserId * string list

type Request =
    | SettingReq of SettingReq
    | ActionReq of ActionReq

module Parser =
    open FParsec

    open DiscordMessage.Parser

    type 'Result Parser = Primitives.Parser<'Result, unit>

    let setSettingName = "setDoorkeeperSettings"

    let psetSetting: _ Parser =
        skipStringCI setSettingName >>. spaces
        >>. (pcodeBlock <|> manySatisfy (fun _ -> true))
        |>> (fun str ->
            try
                let passSettings: Api.TransferTypes.MainData = Api.Serializer.des str
                Result.Ok passSettings
            with e ->
                Result.Error e.Message
        )

    let pgetSetting: _ Parser =
        skipStringCI "getDoorkeeperSettings"

    let psettings =
        choice [
            psetSetting |>> SettingReq.Set
            pgetSetting >>% SettingReq.Get
        ]

    let paction: _ Parser =
        let ppass: _ Parser =
            skipStringCI "pass" >>. spaces
            >>. tuple2
                    (puserMention .>> spaces)
                    (many (many1Satisfy (fun _ -> true)))

        choice [
            ppass |>> Pass
        ]

    let start f: _ Parser =
        choice [
            paction |>> Request.ActionReq
            psettings |>> Request.SettingReq
        ]
        >>= fun msg ->
            preturn (fun x -> f x msg)

type Msg =
    | Request of EventArgs.MessageCreateEventArgs * Request
    | GuildMemberAddedHandle of EventArgs.GuildMemberAddEventArgs
    | GuildMemberRemovedHandle of EventArgs.GuildMemberRemoveEventArgs
    | ApiRequest of Api.Request * AsyncReplyChannel<Api.Response>

let actionReduce
    (e: EventArgs.MessageCreateEventArgs)
    (msg: ActionReq)
    (state: State): State =

    let settings = state.Settings

    match msg with
    | Pass (targetUserId, roleKeys) ->
        awaiti <| e.Channel.TriggerTypingAsync()

        let sendMessage msg =
            let embed = Entities.DiscordEmbedBuilder()
            embed.Color <- Entities.Optional.FromValue(DiscordEmbed.backgroundColorDarkTheme)
            embed.Description <- msg

            let b = Entities.DiscordMessageBuilder()
            b.Embed <- embed.Build()

            awaiti <| e.Channel.SendMessageAsync b

        let getEnabledOptionValue name enabledOptionValue next =
            let isEnabledDoorkeeperRole next =
                if enabledOptionValue.IsEnabled then
                    next ()
                else
                    sendMessage (
                        sprintf "%s is disabled" name
                    )

            let getDoorkeeperRole next =
                match enabledOptionValue.Value with
                | Some permittedRole ->
                    next permittedRole
                | None ->
                    sendMessage (
                        sprintf "%s is not set" name
                    )

            isEnabledDoorkeeperRole <| fun () ->
            getDoorkeeperRole <| next

        let guild = e.Guild

        let getGuildMember userId next =
            match await (guild.GetMemberAsync userId) with
            | null ->
                sendMessage (
                    sprintf "User <@!%d> is not a member of this guild" userId
                )
            | guildMember ->
                next guildMember

        let getChannel name channelId next =
            match guild.GetChannel channelId with
            | null ->
                sendMessage (
                    sprintf "%s(<#%d>) not found" name channelId
                )

            | mainChannel ->
                next mainChannel

        let getSetting next =
            match Setting.GuildData.tryFind guild.Id settings with
            | Some newcomersRoles ->
                next newcomersRoles
            | None ->
                sendMessage (
                    [
                        "This server doesn't yet have doorkeeper settings. To set them, use the command:"
                        "```"
                        sprintf ".%s" Parser.setSettingName
                        // todo: exception: max length 4096
                        // Setting.MainData.Sample |> Setting.MainData.Serialize
                        "```"
                    ] |> String.concat "\n"
                )

        let isAllowedUseCommandInThisChannel (allowedChannelId: ChannelId) next =
            if allowedChannelId = e.Channel.Id then
                next ()
            else
                sendMessage (
                    sprintf "This command is allowed to use only in <#%d>" allowedChannelId
                )

        let isCurrentUserHasRight roleId next =
            let currentMember = Types.getGuildMember guild e.Author

            if currentMember.Roles |> Seq.exists (fun x -> x.Id = roleId) then
                next ()
            else
                sprintf "This command is only allowed for users who have <@&%d> role" roleId
                |> sendMessage

        let issuedRolesHandle (checkpoint: Setting.Checkpoint) next =
            let getIssuedRoles (issuedRoleIds: (string * RoleId) []) next =
                let roleKeys =
                    let rec f acc = function
                        | roleKey::roleKeys ->
                            let res =
                                issuedRoleIds
                                |> Array.tryFind (fun (key', _) -> key' = roleKey)

                            match res with
                            | Some roleId -> f (roleId::acc) roleKeys
                            | None -> Error roleKey

                        | [] -> Ok acc

                    f [] roleKeys

                match roleKeys with
                | Ok roleKeys ->
                    next roleKeys

                | Error notFoundKey ->
                    sendMessage (
                        sprintf
                            "Not found \"%s\" key. Available next keys:\n%s"
                            notFoundKey
                            (issuedRoleIds
                             |> Array.map (fun (k, roleId) ->
                                sprintf "* %s <@&%d>" k roleId)
                             |> String.concat "\n")
                    )

            if List.isEmpty roleKeys then
                next None
            else
                getEnabledOptionValue "checkpoint.IssuedRoleIds" checkpoint.IssuedRoleIds <| fun issuedRoleIds ->
                getIssuedRoles issuedRoleIds <| fun roleKeys ->
                next (Some roleKeys)

        let revokeRole (roleId: RoleId) (targetUser: Entities.DiscordMember) =
            match guild.GetRole roleId with
            | null -> ()
            | role ->
                try
                    awaiti <| targetUser.RevokeRoleAsync(role)
                with e ->
                    sendMessage (
                        sprintf "Error when revoked <@&%d>: %s" roleId e.Message
                    )

        let addIssuedRoles roleKeys (targetUser: Entities.DiscordMember) =
            roleKeys
            |> List.iter (fun (key, roleId) ->
                match guild.GetRole roleId with
                | null ->
                    sendMessage (
                        sprintf "Role %s (%d) is not exists" key roleId
                    )
                | role ->
                    try
                        awaiti <| targetUser.GrantRoleAsync(role)
                    with e ->
                        sendMessage (
                            sprintf "Error when grunted <@&%d>: %s" roleId e.Message
                        )
            )

        let getUserData targetUserId =
            let id = Leavers.Id.create guild.Id targetUserId
            Leavers.GuildUsers.tryFindById id state.Leavers

        let returnAllRoles (setting: Setting.MainData) (userData: Leavers.GuildUser) (targetUser: Entities.DiscordMember) =
            let enteredUserRole = EnabledOptionValue.toOption setting.Checkpoint.EnteredUserRole
            let returnedUserExcludeRoles = EnabledOptionValue.toOption setting.Inner.ReturnedUserExcludeRoles

            let isEnteredUserRoleId =
                match enteredUserRole with
                | Some enteredUserRoleId ->
                    (=) enteredUserRoleId
                | None ->
                    fun _ -> false

            let isReturnedUserExcludeRoleId =
                match returnedUserExcludeRoles with
                | Some returnedUserExcludeRoles ->
                    fun x -> Seq.contains x returnedUserExcludeRoles
                | None ->
                    fun _ -> false

            userData.Data.RoleIds
            |> Array.iter (fun roleId ->
                if not (isEnteredUserRoleId roleId || isReturnedUserExcludeRoleId roleId) then
                    grantRoleSilent guild roleId targetUser
            )

        let sendWelcomeMessage targetUser isUserNewcomer (setting: Setting.MainData) =
            let send = send guild targetUser
            let sendRandom = sendRandom guild targetUser
            if isUserNewcomer then
                let inner = setting.Inner
                sendRandom inner.NewcomerWelcomeMessage inner.Channel

                let log = setting.Log
                send inner.NewcomerWelcomeMessageLog log.Channel
            else
                let inner = setting.Inner
                sendRandom inner.ReturnedWelcomeMessage inner.Channel

                let log = setting.Log
                send inner.ReturnedWelcomeMessageLog log.Channel

        getSetting <| fun setting ->
        let checkpoint = setting.Checkpoint

        getEnabledOptionValue "checkpoint.Channel" checkpoint.Channel <| fun checkpointChannelId ->
        isAllowedUseCommandInThisChannel checkpointChannelId <| fun () ->

        getEnabledOptionValue "checkpoint.DoorkeeperRole" checkpoint.DoorkeeperRole <| fun doorkeeperRoleId ->
        isCurrentUserHasRight doorkeeperRoleId <| fun () ->

        issuedRolesHandle checkpoint <| fun roleKeys ->

        getGuildMember targetUserId <| fun targetUser ->

        getEnabledOptionValue "checkpoint.EnteredUserRole" checkpoint.EnteredUserRole <| fun enteredUserRoleId ->
            revokeRole enteredUserRoleId targetUser

        let userData = getUserData targetUser.Id

        userData
        |> Option.iter (fun userData ->
            returnAllRoles setting userData targetUser
        )

        roleKeys
        |> Option.iter (fun roleKeys ->
            addIssuedRoles roleKeys targetUser)

        let isUserNewcomer = Option.isNone userData
        sendWelcomeMessage targetUser isUserNewcomer (setting: Setting.MainData)

        sendMessage "Done!"

    state

let settingReduce
    (e: EventArgs.MessageCreateEventArgs)
    (msg: SettingReq)
    (settings: State): State =

    awaiti <| e.Channel.TriggerTypingAsync()

    let sendMessage msg =
        let embed = Entities.DiscordEmbedBuilder()
        embed.Color <- Entities.Optional.FromValue(DiscordEmbed.backgroundColorDarkTheme)
        embed.Description <- msg

        let b = Entities.DiscordMessageBuilder()
        b.Embed <- embed.Build()

        awaiti <| e.Channel.SendMessageAsync b

    let guild = e.Guild

    match msg with
    | SettingReq.Set settingOpt ->
        match settingOpt with
        | Ok setting ->
            let currentMember = getGuildMember e.Guild e.Author

            settings
            |> isUserHasAdministrativeRight sendMessage currentMember ^<| fun () settings ->

            let settings =
                settings
                |> Api.set (Api.Snowflake.Create guild.Id, setting)

            sendMessage "Done!"

            settings

        | Error errMsg ->
            sendMessage errMsg

            settings

    | SettingReq.Get ->
        let getSetting (guildId: GuildId) next =
            match Api.get (Api.Snowflake.Create guildId) settings with
            | Some newcomersRoles ->
                next newcomersRoles
            | None ->
                sendMessage (
                    [
                        "This server doesn't yet have doorkeeper settings. To set them, use the command:"
                        "```"
                        sprintf ".%s" Parser.setSettingName
                        Setting.MainData.Sample |> Api.Serializer.ser
                        "```"
                    ] |> String.concat "\n"
                )

        getSetting guild.Id <| fun setting ->
            sendMessage (
                sprintf "```json\n%s\n```" (Api.Serializer.ser setting)
            )

        settings

let reduce (msg: Msg) (state: State): State =
    match msg with
    | GuildMemberAddedHandle e ->
        let currentUser = e.Member
        let guild = e.Guild
        let guildId = guild.Id

        let getUserData targetUserId =
            let id = Leavers.Id.create guild.Id targetUserId
            Leavers.GuildUsers.tryFindById id state.Leavers

        state
        |> isUserNotBot currentUser ^<| fun () ->

        getSettingSilent guildId state.Settings <| fun setting state ->
            let checkpoint = setting.Checkpoint

            getEnabledOptionValueSilent checkpoint.EnteredUserRole <| fun roleIds ->
                grantRoleSilent guild roleIds currentUser

            let send = send guild currentUser
            let sendRandom = sendRandom guild currentUser
            let log = setting.Log
            match getUserData currentUser.Id with
            | None ->
                sendRandom checkpoint.NewcomerWelcomeMessage checkpoint.Channel

                send checkpoint.NewcomerWelcomeMessageLog log.Channel

            | Some userData ->
                sendRandom checkpoint.ReturnedWelcomeMessage checkpoint.Channel

                match EnabledOptionValue.toOption checkpoint.ReturnedUserIncludeRoles with
                | None -> ()
                | Some returnedUserIncludeRoles ->
                    if not <| Array.isEmpty returnedUserIncludeRoles then
                        userData.Data.RoleIds
                        |> Array.iter (fun roleId ->
                            if Array.contains roleId returnedUserIncludeRoles then
                                grantRoleSilent guild roleId currentUser
                        )

                send checkpoint.ReturnedWelcomeMessageLog log.Channel

            state

    | GuildMemberRemovedHandle e ->
        let currentUser = e.Member
        let guild = e.Guild
        let guildId = guild.Id

        state
        |> isUserNotBot currentUser ^<| fun () ->

        getSettingSilent guildId state.Settings ^<| fun setting state ->

        getEnabledOptionValueSilent setting.Checkpoint.EnteredUserRole <| fun enteredRole ->
            let isUserInCheckpoint () =
                e.Member.Roles
                |> Seq.exists (fun x -> x.Id = enteredRole)

            let send = send guild currentUser
            let sendRandom = sendRandom guild currentUser
            let log = setting.Log
            if isUserInCheckpoint () then
                let checkpoint = setting.Checkpoint
                sendRandom checkpoint.GoodbyeMessage checkpoint.Channel

                send checkpoint.GoodbyeMessageLog log.Channel
            else
                let exit = setting.Exit
                sendRandom exit.GoodbyeMessage exit.Channel

                send exit.GoodbyeMessageLog log.Channel

        { state with
            Leavers =
                let roleIds =
                    e.Member.Roles
                    |> Seq.map (fun role -> role.Id)
                    |> Array.ofSeq

                let id = Leavers.Id.create guild.Id e.Member.Id
                state.Leavers
                |> Leavers.GuildUsers.set
                    id
                    (fun data ->
                        { data with
                            RoleIds = roleIds
                        }
                    )
        }

    | Request(e, cmd) ->
        match cmd with
        | Request.ActionReq msg ->
            actionReduce e msg state

        | Request.SettingReq msg ->
            settingReduce e msg state

    | ApiRequest (req, replyChannel) ->
        let newState, res = Api.requestHandler state req

        replyChannel.Reply res

        match newState with
        | Some newState -> newState
        | None -> state

let m =
    let init = {
        Settings = Setting.GuildData.init Db.database
        Leavers = Leavers.GuildUsers.init "leavers" Db.database
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

let exec: MessageCreateEventHandler Parser.Parser =
    Parser.start (fun (client: DiscordClient, e: EventArgs.MessageCreateEventArgs) msg ->
        m.Post (Request (e, msg))
    )

let apiRequestHandle req =
    m.PostAndReply (fun r -> ApiRequest(req, r))
