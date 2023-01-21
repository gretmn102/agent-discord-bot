module UserRole.Main
open FsharpMyExtension
open FsharpMyExtension.Either
open DSharpPlus

open Shared
open Types
open Extensions
open Model

module AbstrRoleList =
    type Cmd =
        | AddRoleId of RoleId
        | RemoveRoleId of RoleId
        | GetRoles

    module Parser =
        open FParsec

        open DiscordMessage.Parser

        type 'Result Parser = Primitives.Parser<'Result, unit>

        let create cmdName =
            let paddRole: RoleId Parser =
                pstringCI (sprintf "add%sRole" cmdName) >>. spaces
                >>. (pmentionRole <|> puint64)

            let premoveRole: RoleId Parser =
                pstringCI (sprintf "remove%sRole" cmdName) >>. spaces
                >>. (pmentionRole <|> puint64)

            let pgetRoles: _ Parser =
                pstringCI (sprintf "%sRoles" cmdName)

            let start: _ Parser =
                choice [
                    paddRole |>> AddRoleId
                    premoveRole |>> RemoveRoleId
                    pgetRoles >>% GetRoles
                ]

            start

    let reducer get update (e: EventArgs.MessageCreateEventArgs) (state: Setting.GuildData) cmd =
        match cmd with
        | AddRoleId roleId ->
            let replyMessage =
                await (e.Channel.SendMessageAsync("Processing..."))

            let guild = e.Guild
            let currentMember = getGuildMember guild e.Author

            if currentMember.Permissions &&& Permissions.ManageRoles = Permissions.ManageRoles then
                let state =
                    state
                    |> Setting.GuildData.set
                        guild.Id
                        (fun setting ->
                            setting
                            |> update (Set.add roleId (get setting))
                        )

                awaiti (replyMessage.ModifyAsync(Entities.Optional("Role has been added")))

                state
            else
                awaiti (replyMessage.ModifyAsync(Entities.Optional("You don't have permission to manage roles")))

                state

        | RemoveRoleId roleId ->
            let replyMessage =
                await (e.Channel.SendMessageAsync("Processing..."))

            let guild = e.Guild
            let guildId = guild.Id
            let currentMember = getGuildMember guild e.Author

            if currentMember.Permissions &&& Permissions.ManageRoles = Permissions.ManageRoles then
                match Setting.GuildData.tryFind guildId state with
                | Some permissiveRoles ->
                    if Set.contains roleId permissiveRoles.PermissiveRoleIds then
                        let state =
                            state
                            |> Setting.GuildData.set
                                guildId
                                (fun setting ->
                                    setting
                                    |> update (Set.remove roleId (get setting))
                                )

                        awaiti (replyMessage.ModifyAsync(Entities.Optional("Role has been removed")))

                        state
                    else
                        awaiti (replyMessage.ModifyAsync(Entities.Optional("Role doesn't exists in DB")))
                        state
                | None ->
                    awaiti (replyMessage.ModifyAsync(Entities.Optional("This server doesn't yet have permissive roles")))
                    state

            else
                awaiti (replyMessage.ModifyAsync(Entities.Optional("You don't have permission to manage roles")))

                state

        | GetRoles ->
            let message =
                match Setting.GuildData.tryFind e.Guild.Id state with
                | Some setting ->
                    let b = Entities.DiscordMessageBuilder()
                    let embed = Entities.DiscordEmbedBuilder()
                    embed.Color <- Entities.Optional.FromValue(DiscordEmbed.backgroundColorDarkTheme)
                    embed.Description <-
                        [
                            yield "Permissive roles:"
                            yield! get setting |> Set.map (sprintf "* <@&%d>")
                        ] |> String.concat "\n"

                    b.Embed <- embed.Build()
                    b
                | None ->
                    let b = Entities.DiscordMessageBuilder()
                    let embed = Entities.DiscordEmbedBuilder()
                    embed.Description <-
                        [
                            "This server doesn't yet have permissive roles. To add them, use the command:"
                            "```"
                            ".addPermissiveRole <role_mention|role_id>"
                            "```"
                        ] |> String.concat "\n"
                    b.Embed <- embed.Build()
                    b

            awaiti (e.Channel.SendMessageAsync message)

            state

    /// `Some _` if something changes
    let roleDeleteHandler get update (e: EventArgs.GuildRoleDeleteEventArgs) (state: Setting.GuildData) =
        let guildId = e.Role.Id
        let roleId = e.Role.Id

        match Setting.GuildData.tryFind guildId state with
        | Some permissiveRoles ->
            if Set.contains roleId permissiveRoles.PermissiveRoleIds then
                state
                |> Setting.GuildData.set
                    guildId
                    (fun setting ->
                        setting
                        |> update (Set.remove roleId (get setting))
                    )
                |> Some
            else
                None
        | None ->
            None

module PermissiveRoleList =
    open AbstrRoleList

    let private get (setting: Setting.MainData) =
        setting.PermissiveRoleIds

    let private update newVal (setting: Setting.MainData) =
        { setting with
            PermissiveRoleIds = newVal
        }

    module Parser =
        let start = Parser.create "permissive"

    let reducer e state cmd =
        reducer
            get
            update
            e
            state
            cmd

    let roleDeleteHandler e state =
        roleDeleteHandler
            get
            update
            e
            state

module PermissiveIconRoleList =
    open AbstrRoleList

    let private get (setting: Setting.MainData) =
        setting.PermissiveIconRoleIds

    let private update newVal (setting: Setting.MainData) =
        { setting with
            PermissiveIconRoleIds = newVal
        }

    module Parser =
        let start = Parser.create "permissiveIcon"

    let reducer e state cmd =
        reducer
            get
            update
            e
            state
            cmd

    let roleDeleteHandler e state =
        roleDeleteHandler
            get
            update
            e
            state

type RoleEditModel = {
    Name: string
    Color: Entities.DiscordColor
    IconUrl: string option
}

type Request =
    | GiveOrChangeRole of RoleEditModel option
    | PermissiveRoleCmd of AbstrRoleList.Cmd
    | PermissiveIconRoleCmd of AbstrRoleList.Cmd
    | GetUserRoles
    | RemoveUserRole of RoleId
    | UpdateRolesPermission
    | SetTemplateRole of RoleId
    | GetTemplateRole
    | SetUserRoleToUser of RoleId * UserId

type Req =
    | Request of EventArgs.MessageCreateEventArgs * Request
    | GuildRoleDeletedHandler of EventArgs.GuildRoleDeleteEventArgs
    | ModalHandle of AsyncReplyChannel<bool> * EventArgs.ModalSubmitEventArgs
    | ComponentInteractionCreateHandle of AsyncReplyChannel<bool> * DiscordClient * EventArgs.ComponentInteractionCreateEventArgs

type State =
    {
        Setting: Setting.GuildData
        GuildUserRoles: Roles.GuildUsers
    }

module Parser =
    open FParsec

    open DiscordMessage.Parser

    type 'Result Parser = Primitives.Parser<'Result, unit>

    let phexColor: _ Parser =
        pchar '#'
        >>. manyMinMaxSatisfy 1 6 isHex
        |>> fun hex ->
            System.Convert.ToInt32(hex, 16)
            |> Entities.DiscordColor

    let phexOrDecColor: _ Parser =
        phexColor <|> (pint32 |>> Entities.DiscordColor)

    let roleNameCmd = "role"

    let pgiveOrChangeRole: _ Parser =
        let pargs =
            pipe3
                (pquote .>> spaces)
                (phexColor .>> spaces)
                (opt (many1Satisfy (fun _ -> true))) // todo: validate url
                (fun name color iconUrl ->
                    {
                        Name = name
                        Color = color
                        IconUrl = iconUrl
                    }
                )

        pstringCI roleNameCmd >>. spaces
        >>. opt pargs

    let pgetUserRoles: _ Parser =
        pstringCI "userRoles"

    let premoveUserRole: _ Parser =
        pstringCI "removeUserRole" >>. spaces
        >>. (pmentionRole <|> puint64)

    let psetTemplateRole: _ Parser =
        pstringCI "setTemplateRole" >>. spaces
        >>. (pmentionRole <|> puint64)

    let pgetTemplateRole: _ Parser =
        pstringCI "getTemplateRole"

    let pupdateUserRolesPermissions: _ Parser =
        pstringCI "updateUserRolesPermissions" >>. spaces

    let psetUserRoleToUser: _ Parser =
        pstringCI "setUserRoleToUser" >>. spaces
        >>. tuple2
                ((pmentionRole <|> puint64) .>> spaces)
                (puserMention <|> puint64)

    let start f: _ Parser =
        choice [
            pgiveOrChangeRole |>> GiveOrChangeRole
            premoveUserRole |>> RemoveUserRole
            pgetUserRoles >>% GetUserRoles
            psetUserRoleToUser |>> SetUserRoleToUser

            psetTemplateRole |>> SetTemplateRole
            pgetTemplateRole >>% GetTemplateRole
            pupdateUserRolesPermissions >>% UpdateRolesPermission

            PermissiveRoleList.Parser.start |>> PermissiveRoleCmd

            PermissiveIconRoleList.Parser.start |>> PermissiveIconRoleCmd
        ]
        >>= fun msg ->
            preturn (fun x -> f x msg)

module UserRoleForm =
    [<Literal>]
    let GiveOrChangeRoleButtonId = "GiveOrChangeRoleButtonId"

    [<Literal>]
    let UserRoleFormModalId = "UserRoleFormModalId"

    [<Literal>]
    let UserRoleFormModalRoleNameId = "UserRoleFormModalRoleNameId"

    [<Literal>]
    let UserRoleFormModalColorId = "UserRoleFormModalColorId"

    [<Literal>]
    let UserRoleFormModalIconUrl = "UserRoleFormModalIconUrl"

    let createUI
        (addComponents: Entities.DiscordComponent [] -> unit)
        addEmbed
        (guildMember: Entities.DiscordMember)
        (userRole: UserId option) =

        let content =
            match userRole with
            | Some userRole -> sprintf "%s, <@&%d> — твоя пользовательская роль." guildMember.Mention userRole
            | None -> sprintf "%s, у тебя пока что нет пользовательской роли." guildMember.Mention

        let embed =
            Entities.DiscordEmbedBuilder()
                .WithAuthor(sprintf "Пользовательская роль", iconUrl = "https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/120/google/313/performing-arts_1f3ad.png")
                .WithColor(DiscordEmbed.backgroundColorDarkTheme)
                .WithDescription(content)
                // .WithFooter(sprintf "OwnerId: %d" guildMember.Id)
                .Build()

        addEmbed embed

        addComponents [|
            Entities.DiscordButtonComponent(
                ButtonStyle.Secondary,
                sprintf "%s%d" GiveOrChangeRoleButtonId guildMember.Id,
                (match userRole with None -> "Создать роль" | Some _ -> "Изменить роль"),
                emoji = Entities.DiscordComponentEmoji(Name = "🎭")
            )
        |]

    let componentInteractionCreateHandle (existRole: RoleEditModel option) isIconAllowed (client: DiscordClient) (e: EventArgs.ComponentInteractionCreateEventArgs) =
        if e.Message.Author.Id = client.CurrentUser.Id then
            if e.Id.StartsWith GiveOrChangeRoleButtonId then
                let ownerId = uint64 e.Id.[GiveOrChangeRoleButtonId.Length..]
                if e.User.Id = ownerId then
                    let b =
                        Entities.DiscordInteractionResponseBuilder()
                            .WithTitle("Пользовательская роль")
                            .WithCustomId(UserRoleFormModalId)
                            .WithContent("Цвет роли должен быть в шестнадцатеричном формате (например, `#ffa500` даст оранжевый цвет). Его можно выбрать на [W3Schools](https://www.w3schools.com/colors/colors_picker.asp), например, или еще где-нибудь.")
                            .AddComponents([|
                                Entities.TextInputComponent(
                                    "Название роли",
                                    UserRoleFormModalRoleNameId,
                                    required = true,
                                    placeholder = "Главная роль!",
                                    style = TextInputStyle.Short,
                                    value = (match existRole with None -> "" | Some role -> role.Name)
                                )

                            |]: Entities.DiscordComponent [])
                            .AddComponents(
                                Entities.TextInputComponent(
                                    "Цвет роли",
                                    UserRoleFormModalColorId,
                                    required = true,
                                    placeholder = "#ffa500",
                                    style = TextInputStyle.Short,
                                    value = (match existRole with None -> "" | Some role -> sprintf "#%x" role.Color.Value)
                                )
                            )

                    let b =
                        if isIconAllowed then
                            b.AddComponents(
                                Entities.TextInputComponent(
                                    "Ссылка на иконку",
                                    UserRoleFormModalIconUrl,
                                    required = false,
                                    placeholder = "",
                                    style = TextInputStyle.Short,
                                    value = (match existRole with None -> "" | Some role -> match role.IconUrl with None -> "" | Some x -> x)
                                )
                            )
                        else
                            b

                    awaiti <| e.Interaction.CreateResponseAsync(InteractionResponseType.Modal, b)
                else
                    let b =
                        Entities.DiscordInteractionResponseBuilder()

                    b.IsEphemeral <- true
                    b.Content <- sprintf "Этот интерфейс принадлежит <@!%d>. Создайте себе свой командой `.%s`" ownerId Parser.roleNameCmd

                    awaiti <| e.Interaction.CreateResponseAsync(InteractionResponseType.ChannelMessageWithSource, b)

                true
            else
                false
        else
            false

    let giveOrChangeRole
        userId
        (guild: Entities.DiscordGuild)
        setContent
        (addComponents: Entities.DiscordComponent [] -> unit)
        addEmbed
        sendMessage
        roleGranted
        (roleEditModel: RoleEditModel option)
        (state: State) =

        let guildUserRoles = state.GuildUserRoles

        let guildMember = await (guild.GetMemberAsync userId)

        match Setting.GuildData.tryFind guild.Id state.Setting with
        | Some setting ->
            let hasPermissiveRole =
                guildMember.Roles
                |> Seq.exists (fun role ->
                    Set.contains role.Id setting.PermissiveRoleIds)

            if hasPermissiveRole then
                let createAndGrantRole () =
                    match setting.TemplateRoleId with
                    | Some templateRoleId ->
                        let templateRole = guild.GetRole templateRoleId

                        if isNull templateRole then
                            setContent "The guild owner installed the template role, but it has been removed"

                            guildUserRoles
                        else
                            if guild.Roles.Count < 250 then
                                match roleEditModel with
                                | None ->
                                    createUI addComponents addEmbed guildMember None

                                    guildUserRoles
                                | Some roleEditModel ->
                                    let res =
                                        let changeWithoutIcon () =
                                            try
                                                guild.CreateRoleAsync (
                                                    name = roleEditModel.Name,
                                                    color = System.Nullable(roleEditModel.Color),
                                                    permissions = System.Nullable templateRole.Permissions
                                                )
                                                |> await
                                                |> Right
                                            with e ->
                                                Left e.Message

                                        let changeWithIcon iconUrl =
                                            if guild.Features |> Seq.exists ((=) "ROLE_ICONS") then
                                                let hasPermissiveRole =
                                                    guildMember.Roles
                                                    |> Seq.exists (fun role ->
                                                        Set.contains role.Id setting.PermissiveIconRoleIds)

                                                if hasPermissiveRole then
                                                    match WebClientDownloader.getData [] iconUrl with
                                                    | Left errMsg ->
                                                        sprintf "Download url by '%s' error:\n%s" iconUrl errMsg
                                                        |> Left
                                                    | Right bytes ->
                                                        try
                                                            use m = new System.IO.MemoryStream(bytes)

                                                            guild.CreateRoleAsync (
                                                                name = roleEditModel.Name,
                                                                color = System.Nullable(roleEditModel.Color),
                                                                icon = m,
                                                                permissions = System.Nullable templateRole.Permissions
                                                            )
                                                            |> await
                                                            |> Right
                                                        with e ->
                                                            Left e.Message
                                                else
                                                    sprintf "You do not have these permissive icon roles: %s."
                                                        (setting.PermissiveIconRoleIds |> Seq.map (sprintf "<@&%d>") |> String.concat ", ")
                                                    |> Left
                                            else
                                                Left "This guild don't has ROLE_ICONS feature"

                                        match roleEditModel.IconUrl with
                                        | Some iconUrl ->
                                            changeWithIcon iconUrl
                                        | None -> changeWithoutIcon ()

                                    match res with
                                    | Right role ->
                                        try
                                            awaiti <| role.ModifyPositionAsync(templateRole.Position - 1)
                                        with
                                            ex ->
                                                let jsonMessage =
                                                    match ex with
                                                    | :? Exceptions.BadRequestException as x ->
                                                        x.JsonMessage
                                                    | :? Exceptions.UnauthorizedException as x ->
                                                        x.JsonMessage
                                                    | x -> x.Message

                                                let embed =
                                                    Entities.DiscordEmbedBuilder()
                                                        .WithColor(DiscordEmbed.backgroundColorDarkTheme)
                                                        .WithDescription(
                                                            [
                                                                "Error:"
                                                                "```"
                                                                sprintf "%s" jsonMessage
                                                                "```"
                                                                sprintf "The bot role must be higher than <@&%d>, or move the role yourself" templateRole.Id
                                                            ] |> String.concat "\n"
                                                        ).Build()

                                                sendMessage embed

                                        guildMember.GrantRoleAsync role
                                        |> fun t -> t.GetAwaiter() |> fun x -> x.GetResult()

                                        roleGranted role.Id
                                        setContent "Role has been granted"

                                        guildUserRoles
                                        |> Roles.GuildUsers.set
                                            (Roles.Id.create guild.Id guildMember.Id)
                                            (fun user ->
                                                { user with
                                                    RoleId = role.Id
                                                }
                                            )

                                    | Left errMsg ->
                                        setContent errMsg

                                        guildUserRoles
                            else
                                setContent "The number of roles exceeds 250"

                                guildUserRoles
                    | None ->
                        setContent "The guild owner didn't set the template role"

                        guildUserRoles

                let id = Roles.Id.create guild.Id guildMember.Id
                match Roles.GuildUsers.tryFindById id guildUserRoles with
                | Some roleData ->
                    match guild.GetRole roleData.Data.RoleId with
                    | null ->
                        createAndGrantRole ()
                    | userRole ->
                        match roleEditModel with
                        | None ->
                            createUI addComponents addEmbed guildMember (Some userRole.Id)

                            guildUserRoles

                        | Some roleEditModel ->
                            let res =
                                let res =
                                    let changeWithoutIcon () =
                                        try
                                            userRole.ModifyAsync (
                                                name = roleEditModel.Name,
                                                color = System.Nullable(roleEditModel.Color)
                                            )
                                            |> fun t -> t.GetAwaiter() |> fun x -> x.GetResult()

                                            Right ()
                                        with e ->
                                            Left e.Message

                                    let changeWithIcon iconUrl =
                                        if guild.Features |> Seq.exists ((=) "ROLE_ICONS") then
                                            let hasPermissiveRole =
                                                guildMember.Roles
                                                |> Seq.exists (fun role ->
                                                    Set.contains role.Id setting.PermissiveIconRoleIds)

                                            if hasPermissiveRole then
                                                match WebClientDownloader.getData [] iconUrl with
                                                | Left errMsg ->
                                                    sprintf "Download url by '%s' error:\n%s" iconUrl errMsg
                                                    |> Left
                                                | Right bytes ->
                                                    try
                                                        use m = new System.IO.MemoryStream(bytes)

                                                        userRole.ModifyAsync (
                                                            name = roleEditModel.Name,
                                                            color = System.Nullable(roleEditModel.Color),
                                                            icon = m
                                                        )
                                                        |> fun t -> t.GetAwaiter() |> fun x -> x.GetResult()

                                                        Right ()
                                                    with e ->
                                                        Left e.Message
                                            else
                                                sprintf "You do not have these permissive icon roles: %s."
                                                    (setting.PermissiveIconRoleIds |> Seq.map (sprintf "<@&%d>") |> String.concat ", ")
                                                |> Left
                                        else
                                            Left "This guild don't has ROLE_ICONS feature"

                                    match roleEditModel.IconUrl with
                                    | Some iconUrl ->
                                        if userRole.IconUrl = iconUrl then
                                            changeWithoutIcon ()
                                        else
                                            changeWithIcon iconUrl
                                    | None -> changeWithoutIcon ()

                                match res with
                                | Right () ->
                                    let roleGranded =
                                        guildMember.Roles
                                        |> Seq.exists (fun x -> x.Id = userRole.Id)

                                    if roleGranded then
                                        "Role has been changed"
                                    else
                                        try
                                            guildMember.GrantRoleAsync userRole
                                            |> fun t -> t.GetAwaiter() |> fun x -> x.GetResult()

                                            "Role has been changed and returned to user"
                                        with e ->
                                            sprintf "An error occurred when returning the role to the user:\n%s" e.Message

                                | Left errMsg -> errMsg

                            setContent res

                            guildUserRoles
                | None ->
                    createAndGrantRole ()

            else
                let embed =
                    Entities.DiscordEmbedBuilder()
                        .WithColor(DiscordEmbed.backgroundColorDarkTheme)
                        .WithDescription(
                            sprintf "%s, you do not have these permissive roles: %s."
                                guildMember.Mention
                                (setting.PermissiveRoleIds |> Seq.map (sprintf "<@&%d>") |> String.concat ", ")
                            )
                        .Build()

                addEmbed embed

                guildUserRoles

        | None ->
            let embed =
                Entities.DiscordEmbedBuilder()
                    .WithColor(DiscordEmbed.backgroundColorDarkTheme)
                    .WithDescription(
                        [
                            "This server doesn't yet have permissive roles. To add them, use the command:"
                            "```"
                            ".addPermissiveRole <role_mention|role_id>"
                            "```"
                        ] |> String.concat "\n")
                    .Build()

            addEmbed embed

            guildUserRoles

    let modalHandle (e: EventArgs.ModalSubmitEventArgs) (state: State) =
        let interaction = e.Interaction
        if interaction.Data.CustomId = UserRoleFormModalId then
            let name =
                match e.Values.TryGetValue UserRoleFormModalRoleNameId with
                | true, roleName ->
                    if System.String.IsNullOrWhiteSpace roleName then
                        Left "Название роли не должно быть пустым или состоять из пробелов"
                    else Right roleName
                | false, _ -> Left (sprintf "Internal error: %s not found" UserRoleFormModalRoleNameId)

            let color =
                match e.Values.TryGetValue UserRoleFormModalColorId with
                | true, hexOrDecColorStr ->
                    match FParsecExt.runEither Parser.phexOrDecColor hexOrDecColorStr  with
                    | Left errMsg ->
                        sprintf "Пожалуйста, введите цвет в шестнадцатеричном формате (например, `#ffa500` даст оранжевый цвет), а не `%s`. Цвет можно выбрать на [W3Schools](https://www.w3schools.com/colors/colors_picker.asp), например."
                            hexOrDecColorStr
                        |> Left

                    | Right color -> Right color

                | false, _ -> Left (sprintf "Internal error: %s not found" UserRoleFormModalColorId)

            let iconUrl =
                match e.Values.TryGetValue UserRoleFormModalIconUrl with
                | true, iconUrl ->
                    if System.String.IsNullOrWhiteSpace iconUrl then
                        Right None
                    else
                        // todo: validate url
                        Right (Some iconUrl)
                | false, _ ->
                    Right None

            let state =
                match name, color, iconUrl with
                | Right name, Right color, Right iconUrl ->
                    let b = Entities.DiscordInteractionResponseBuilder()
                    b.IsEphemeral <- true

                    let changed = ref false

                    let guildUserRoles =
                        giveOrChangeRole
                            e.Interaction.User.Id
                            e.Interaction.Guild
                            (fun content -> b.Content <- content)
                            (b.AddComponents >> ignore)
                            (b.AddEmbed >> ignore)
                            (fun embed ->
                                let b =
                                    Entities.DiscordInteractionResponseBuilder()
                                        .AsEphemeral(true)
                                        .AddEmbed(embed)

                                awaiti <| interaction.CreateResponseAsync(InteractionResponseType.ChannelMessageWithSource, b)
                            )
                            (fun userRoleId ->
                                let b = Entities.DiscordInteractionResponseBuilder()

                                let guildMember = getGuildMember e.Interaction.Guild e.Interaction.User

                                createUI (b.AddComponents >> ignore) (b.AddEmbed >> ignore) guildMember (Some userRoleId)
                                awaiti <| interaction.CreateResponseAsync(InteractionResponseType.UpdateMessage, b)

                                changed := true
                            )
                            (Some {
                                Name = name
                                Color = color
                                IconUrl = iconUrl
                            })
                            state

                    if !changed then
                        let b =
                            Entities.DiscordFollowupMessageBuilder()
                                .AsEphemeral(true)
                                .WithContent(b.Content)
                                .AddComponents(b.Components)
                                .AddEmbeds(b.Embeds)

                        awaiti <| interaction.CreateFollowupMessageAsync(b)
                    else
                        awaiti <| interaction.CreateResponseAsync(InteractionResponseType.ChannelMessageWithSource, b)

                    { state with
                        GuildUserRoles = guildUserRoles
                    }
                | _ ->
                    let f = function Left errMsg -> Some errMsg | Right _ -> None

                    let embed =
                        Entities.DiscordEmbedBuilder()
                            .WithColor(DiscordEmbed.backgroundColorDarkTheme)
                            .WithDescription(
                                [f name; f color]
                                |> List.choose id
                                |> String.concat "\n"
                            ).Build()

                    let b =
                        Entities.DiscordInteractionResponseBuilder()
                            .AsEphemeral(true)
                            .AddEmbed(embed)

                    awaiti <| interaction.CreateResponseAsync(InteractionResponseType.ChannelMessageWithSource, b)

                    state

            true, state
        else
            false, state

let reducer (db: MongoDB.Driver.IMongoDatabase) =
    let removeUserRole
        guildId
        roleId
        onRoleHasBeenRemoved
        onRoleDoesNotExists
        onServerDoesNotHaveUserRoles
        (guildUsers: Roles.GuildUsers) =

        match Roles.GuildUsers.tryFindGuildUsers guildId guildUsers with
        | Some userRoles ->
            let guildUser =
                userRoles
                |> Seq.tryPick (fun guildUser ->
                    if guildUser.Data.RoleId = roleId then
                        Some guildUser
                    else
                        None
                )

            match guildUser with
            | Some guildUser ->
                match Roles.GuildUsers.removeById guildUser.Id guildUsers with
                | Some guildUserRoles ->

                    onRoleHasBeenRemoved guildUserRoles
                | None ->
                    failwithf "expected find %A in guildUsers, but not found" guildUser.Id

            | None ->
                onRoleDoesNotExists ()
        | None ->
            onServerDoesNotHaveUserRoles ()

    let reduce msg (state: State) =
        match msg with
        | GuildRoleDeletedHandler e ->
            let guildId = e.Guild.Id
            let roleId = e.Role.Id

            let state =
                let removeUserRole guildUserRoles =
                    guildUserRoles
                    |> removeUserRole
                        guildId
                        roleId
                        id
                        (fun _ -> guildUserRoles)
                        (fun _ -> guildUserRoles)

                { state with GuildUserRoles = removeUserRole state.GuildUserRoles }

            let state =
                match PermissiveRoleList.roleDeleteHandler e state.Setting with
                | Some setting ->
                    { state with
                        Setting = setting
                    }
                | None -> state

            let state =
                match PermissiveIconRoleList.roleDeleteHandler e state.Setting with
                | Some setting ->
                    { state with
                        Setting = setting
                    }
                | None -> state

            state
        | Request(e, msg) ->
            match msg with
            | GiveOrChangeRole roleEditModel ->
                let giveOrChangeRole roleEditModel state =
                    awaiti <| e.Channel.TriggerTypingAsync()

                    let b = Entities.DiscordMessageBuilder()

                    let state =
                        UserRoleForm.giveOrChangeRole
                            e.Author.Id
                            e.Guild
                            (fun content -> b.Content <- content)
                            (b.AddComponents >> ignore)
                            (b.AddEmbed >> ignore)
                            (fun content -> awaiti <| e.Channel.SendMessageAsync content)
                            (fun roleGrantedId -> ())
                            roleEditModel
                            state

                    awaiti <| e.Channel.SendMessageAsync b

                    state

                let roles =
                    try
                        giveOrChangeRole roleEditModel state
                    with e ->
                        printfn "%A" e
                        state.GuildUserRoles

                { state with GuildUserRoles = roles }

            | GetUserRoles ->
                let message =
                    match Roles.GuildUsers.tryFindGuildUsers e.Guild.Id state.GuildUserRoles with
                    | Some userRoles ->
                        let b = Entities.DiscordMessageBuilder()
                        let embed = Entities.DiscordEmbedBuilder()
                        embed.Color <- Entities.Optional.FromValue(DiscordEmbed.backgroundColorDarkTheme)
                        embed.Description <-
                            [
                                yield "User roles: "
                                yield!
                                    userRoles
                                    |> Seq.map (fun userRole ->
                                        sprintf "* <@!%d> — <@&%d> (%d)" userRole.Id.UserId userRole.Data.RoleId userRole.Data.RoleId
                                    )
                            ] |> String.concat "\n"

                        b.Embed <- embed.Build()
                        b
                    | None ->
                        let b = Entities.DiscordMessageBuilder()
                        let embed = Entities.DiscordEmbedBuilder()
                        embed.Description <-
                            [
                                "This server doesn't yet have user roles. To add them, user must have a permissive role and use the command:"
                                "```"
                                ".role \"Role name\" #000000"
                                "```"
                            ] |> String.concat "\n"
                        b.Embed <- embed.Build()
                        b
                awaiti (e.Channel.SendMessageAsync (message))

                state
            | RemoveUserRole userRoleId ->
                let replyMessage =
                    await (e.Channel.SendMessageAsync("Processing..."))

                let guild = e.Guild
                let guildMember = getGuildMember guild e.Author
                let guildUserRoles =
                    if guildMember.Permissions &&& Permissions.ManageRoles = Permissions.ManageRoles then
                        state.GuildUserRoles
                        |> removeUserRole
                            guild.Id
                            userRoleId
                            (fun guildUserRoles ->
                                awaiti (replyMessage.ModifyAsync(Entities.Optional(sprintf "%d role has been removed from DB" userRoleId)))
                                guildUserRoles
                            )
                            (fun () ->
                                awaiti (replyMessage.ModifyAsync(Entities.Optional(sprintf "%d role doesn't exists in DB" userRoleId)))
                                state.GuildUserRoles
                            )
                            (fun () ->
                                awaiti (replyMessage.ModifyAsync(Entities.Optional("This server doesn't yet have user roles")))
                                state.GuildUserRoles
                            )
                    else
                        awaiti (replyMessage.ModifyAsync(Entities.Optional("You don't have permission to manage roles")))
                        state.GuildUserRoles

                { state with GuildUserRoles = guildUserRoles }

            | SetTemplateRole templateRoleId ->
                let replyMessage =
                    await (e.Channel.SendMessageAsync("Processing..."))

                let guild = e.Guild
                let guildMember = getGuildMember guild e.Author
                let templateRoles =
                    if guildMember.Permissions &&& Permissions.ManageRoles = Permissions.ManageRoles then
                        let guildPermissiveRoles =
                            state.Setting
                            |> Setting.GuildData.set
                                guild.Id
                                (fun setting ->
                                    { setting with
                                        TemplateRoleId = Some templateRoleId }
                                )

                        awaiti (replyMessage.ModifyAsync(Entities.Optional("Template role has been set")))

                        guildPermissiveRoles
                    else
                        awaiti (replyMessage.ModifyAsync(Entities.Optional("You don't have permission to manage roles")))
                        state.Setting

                { state with Setting = templateRoles }
            | GetTemplateRole ->
                awaiti <| e.Channel.TriggerTypingAsync()

                let send msg =
                    let embed = Entities.DiscordEmbedBuilder()
                    embed.Color <- Entities.Optional.FromValue(DiscordEmbed.backgroundColorDarkTheme)
                    embed.Description <- msg

                    let b = Entities.DiscordMessageBuilder()
                    b.Embed <- embed.Build()

                    awaiti <| e.Channel.SendMessageAsync b

                let msg =
                    let undefinedTemplateRole =
                        "На этом сервере еще не установлена шаблонная роль."
                    match Setting.GuildData.tryFind e.Guild.Id state.Setting with
                    | Some x ->
                        match x.TemplateRoleId with
                        | Some roleId ->
                            sprintf "<@&%d> — шаблонная роль на данном сервере." roleId
                        | _ ->
                            undefinedTemplateRole
                    | _ ->
                        undefinedTemplateRole

                send msg

                state
            | UpdateRolesPermission ->
                let replyMessage =
                    await (e.Channel.SendMessageAsync("Processing..."))

                let guild = e.Guild
                let guildMember = getGuildMember guild e.Author
                if guildMember.Permissions &&& Permissions.ManageRoles = Permissions.ManageRoles then
                    match Setting.GuildData.tryFind guild.Id state.Setting with
                    | Some templateRole ->
                        match templateRole.TemplateRoleId with
                        | Some templateRoleId ->
                            let templateRole = guild.GetRole templateRoleId

                            if isNull templateRole then
                                awaiti <| replyMessage.ModifyAsync(Entities.Optional("The guild owner installed the template role, but it has been removed"))
                            else
                                match Roles.GuildUsers.tryFindGuildUsers guild.Id state.GuildUserRoles with
                                | Some userRoles ->
                                    userRoles
                                    |> Seq.iter (fun userRole ->
                                        let userRole = guild.GetRole userRole.Data.RoleId
                                        if isNull userRole then ()
                                        else
                                            awaiti <| userRole.ModifyAsync(
                                                permissions = System.Nullable templateRole.Permissions
                                            )

                                            System.Threading.Thread.Sleep 250
                                    )

                                    awaiti (replyMessage.ModifyAsync(Entities.Optional("User roles has been updated")))
                                | None ->
                                    awaiti (replyMessage.ModifyAsync(Entities.Optional("This server doesn't yet have user roles")))
                        | None ->
                            awaiti (replyMessage.ModifyAsync(Entities.Optional("The guild owner didn't set the template role")))
                    | None ->
                        awaiti <| replyMessage.ModifyAsync(Entities.Optional("The guild owner has not set either the template role or permission roles"))
                else
                    awaiti (replyMessage.ModifyAsync(Entities.Optional("You don't have permission to manage roles")))

                state

            | SetUserRoleToUser(roleId, userId) ->
                awaiti <| e.Channel.TriggerTypingAsync()

                let guild = e.Guild
                let guildMember = getGuildMember guild e.Author
                if (guildMember.Permissions &&& Permissions.Administrator = Permissions.Administrator)
                    || (guildMember.Permissions &&& Permissions.ManageRoles = Permissions.ManageRoles) then

                    match guild.GetRole roleId with
                    | null ->
                        awaiti <| e.Channel.SendMessageAsync (sprintf "The %d role not exists in the guild" roleId)

                        state
                    | role ->
                        try
                            let guildMember =
                                if guildMember.Id = userId then guildMember
                                else
                                    await <| guild.GetMemberAsync userId
                            awaiti <| guildMember.GrantRoleAsync(role)
                        with e -> ()

                        let guildUserRoles =
                            let id = Roles.Id.create guild.Id userId
                            state.GuildUserRoles
                            |> Roles.GuildUsers.set
                                id
                                (fun userData ->
                                    { userData with
                                        RoleId = roleId }
                                )

                        { state with
                            GuildUserRoles =
                                guildUserRoles
                        }

                else
                    awaiti <| e.Channel.SendMessageAsync "You don't have permission to manage roles or administrative permission"

                    state

            | PermissiveRoleCmd cmd ->
                { state with
                    Setting =
                        PermissiveRoleList.reducer e state.Setting cmd
                }

            | PermissiveIconRoleCmd cmd ->
                { state with
                    Setting =
                        PermissiveIconRoleList.reducer e state.Setting cmd
                }

        | ModalHandle (r, e) ->
            let isHandled, state = UserRoleForm.modalHandle e state
            r.Reply isHandled
            state

        | ComponentInteractionCreateHandle(r, client, e) ->
            let guild = e.Guild
            let guildMember = getGuildMember guild e.User

            let existRole =
                let userId = guildMember.Id

                let id = Roles.Id.create guild.Id userId

                Roles.GuildUsers.tryFindById id state.GuildUserRoles
                |> Option.bind (fun x ->
                    match guild.GetRole x.Data.RoleId with
                    | null -> None
                    | role ->
                        Some {
                            Name = role.Name
                            Color = role.Color
                            IconUrl =
                                if System.String.IsNullOrEmpty role.IconUrl then
                                    None
                                else
                                    Some role.IconUrl
                        }
                )

            let isIconAllowed =
                let isGuildHasRoleIconsFeature () =
                    guild.Features |> Seq.exists ((=) "ROLE_ICONS")

                let isMemberHasRoleIconPermission () =
                    match Setting.GuildData.tryFind guild.Id state.Setting with
                    | Some setting ->
                        guildMember.Roles
                        |> Seq.exists (fun role ->
                            Set.contains role.Id setting.PermissiveIconRoleIds)
                    | None -> false

                isGuildHasRoleIconsFeature () && isMemberHasRoleIconPermission ()

            r.Reply <| UserRoleForm.componentInteractionCreateHandle existRole isIconAllowed client e

            state

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
        try
            let init =
                {
                    Setting = Setting.GuildData.init db
                    GuildUserRoles = Roles.GuildUsers.init "roles" db
                }
            loop init
        with e ->
            printfn "%A" e
            failwithf "%A" e
    )

let create (db: MongoDB.Driver.IMongoDatabase) =
    let reducer = reducer db

    { BotModule.empty with
        MessageCreateEventHandleExclude =
            let exec: _ Parser.Parser =
                Parser.start (fun (client: DiscordClient, e: EventArgs.MessageCreateEventArgs) msg ->
                    reducer.Post(Request (e, msg))
                )
            Some exec

        ComponentInteractionCreateHandle =
            let componentInteractionCreateHandle ((client: DiscordClient), (e: EventArgs.ComponentInteractionCreateEventArgs)) =
                reducer.PostAndReply (fun r -> ComponentInteractionCreateHandle (r, client, e))
            Some componentInteractionCreateHandle

        ModalSubmit =
            let modalHandle (e: EventArgs.ModalSubmitEventArgs) =
                reducer.PostAndReply (fun r -> ModalHandle(r, e))
            Some modalHandle

        GuildRoleDeletedHandler =
            let guildRoleDeletedHandler (e: EventArgs.GuildRoleDeleteEventArgs) =
                reducer.Post(GuildRoleDeletedHandler e)
            Some guildRoleDeletedHandler
    }
