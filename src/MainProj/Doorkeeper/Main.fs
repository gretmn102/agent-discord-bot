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

open Builder

type SettingReq =
    | Set of Result<Setting.MainData, string>
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
                let passSettings: Setting.MainData = Json.des str
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

    let start: _ Parser =
        choice [
            paction |>> Request.ActionReq
            psettings |>> Request.SettingReq
        ]

type Msg =
    | Request of EventArgs.MessageCreateEventArgs * Request
    | GuildMemberAddedHandle of EventArgs.GuildMemberAddEventArgs
    | GuildMemberRemovedHandle of EventArgs.GuildMemberRemoveEventArgs
    | ApiRequest of Api.Request * AsyncReplyChannel<Api.Response>

let actionReduce
    (e: EventArgs.MessageCreateEventArgs)
    (msg: ActionReq)
    (settings: Setting.GuildData): Setting.GuildData =

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
                        Setting.MainData.Sample |> Setting.MainData.Serialize
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

        roleKeys
        |> Option.iter (fun roleKeys ->
            addIssuedRoles roleKeys targetUser)

        let inner = setting.Inner

        getEnabledOptionValue "Inner.Channel" inner.Channel <| fun innerChannelId ->
            getChannel "Inner.Channel" innerChannelId <| fun innerChannel ->
            getEnabledOptionValue "Inner.NewcomerWelcomeMessage" inner.NewcomerWelcomeMessage <| fun templateMessage ->
            sendTemplateMessage targetUser innerChannel templateMessage

        let log = setting.Log

        getEnabledOptionValueSilent log.Channel <| fun logChannelId ->
            getChannel "log.Channel" logChannelId <| fun innerChannel ->
            getEnabledOptionValue "inner.NewcomerWelcomeMessageLog" inner.NewcomerWelcomeMessageLog <| fun templateMessage ->
            sendTemplateMessage targetUser innerChannel templateMessage

        sendMessage "Done!"

    settings

let settingReduce
    (e: EventArgs.MessageCreateEventArgs)
    (msg: SettingReq)
    (settings: Setting.GuildData): Setting.GuildData =

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
            let settings =
                Setting.GuildData.set
                    guild.Id
                    (fun _ -> setting)
                    settings

            sendMessage "Done!"

            settings

        | Error errMsg ->
            sendMessage errMsg

            settings

    | SettingReq.Get ->
        let getSetting (guildId: GuildId) next =
            match Setting.GuildData.tryFind guildId settings with
            | Some newcomersRoles ->
                next newcomersRoles
            | None ->
                sendMessage (
                    [
                        "This server doesn't yet have doorkeeper settings. To set them, use the command:"
                        "```"
                        sprintf ".%s" Parser.setSettingName
                        Setting.MainData.Sample |> Setting.MainData.Serialize
                        "```"
                    ] |> String.concat "\n"
                )

        getSetting guild.Id <| fun setting ->
            sendMessage (
                sprintf "```json\n%s\n```" (Setting.MainData.Serialize setting)
            )

        settings

let reduce (msg: Msg) (state: State): State =
    match msg with
    | GuildMemberAddedHandle e ->
        let currentUser = e.Member
        let guild = e.Guild
        let guildId = guild.Id

        let getUserData next =
            Map.tryFind guildId state.Leavers
            |> Option.bind (fun userDatas ->
                Map.tryFind currentUser.Id userDatas
                |> Option.map (fun userData -> userData, userDatas)
            )
            |> next

        let send message channel =
            getEnabledOptionValueSilent message <| fun message ->
                getEnabledOptionValueSilent channel <| fun channelId ->
                getChannelSilent channelId guild <| fun channel ->
                sendTemplateMessage currentUser channel message

        state
        |> isUserNotBot currentUser ^<| fun () ->

        getSettingSilent guildId state.Settings <| fun setting state ->
            let checkpoint = setting.Checkpoint

            getEnabledOptionValueSilent checkpoint.EnteredUserRole <| fun roleIds ->
                grantRoleSilent guild roleIds currentUser

            getUserData <| fun userData ->

            match userData with
            | None ->
                send checkpoint.NewcomerWelcomeMessage checkpoint.Channel

                let log = setting.Log
                send checkpoint.NewcomerWelcomeMessageLog log.Channel

            | Some (userData, userDatas) ->
                send checkpoint.ReturnedWelcomeMessage checkpoint.Channel

                userData.RoleIds
                |> Array.iter (fun roleId ->
                    grantRoleSilent guild roleId currentUser
                )

                let log = setting.Log
                send checkpoint.ReturnedWelcomeMessageLog log.Channel

            state

    | GuildMemberRemovedHandle e ->
        let currentUser = e.Member
        let guild = e.Guild
        let guildId = guild.Id

        let send message channel =
            getEnabledOptionValueSilent message <| fun message ->
                getEnabledOptionValueSilent channel <| fun channelId ->
                getChannelSilent channelId guild <| fun channel ->
                sendTemplateMessage currentUser channel message

        state
        |> isUserNotBot currentUser ^<| fun () ->

        getSettingSilent guildId state.Settings ^<| fun setting state ->

        let exit = setting.Exit
        send exit.GoodbyeMessage exit.Channel

        let log = setting.Log
        send exit.GoodbyeMessageLog log.Channel

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

    | Request(e, cmd) ->
        match cmd with
        | Request.ActionReq msg ->
            { state with
                Settings =
                    actionReduce e msg state.Settings
            }

        | Request.SettingReq msg ->
            { state with
                Settings =
                    settingReduce e msg state.Settings
            }

    | ApiRequest (req, replyChannel) ->
        let newState, res = Api.requestHandler state req

        replyChannel.Reply res

        match newState with
        | Some newState -> newState
        | None -> state

let m =
    let init = {
        Settings = Setting.GuildData.init Db.database
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

let apiRequestHandle req =
    m.PostAndReply (fun r -> ApiRequest(req, r))
