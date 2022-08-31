module UserRole.Main
open FsharpMyExtension
open FsharpMyExtension.Either
open DSharpPlus

open Types
open Model

type RoleEditModel = {
    Name: string
    Color: Entities.DiscordColor
    IconUrl: string option
}

type Request =
    | GiveOrChangeRole of RoleEditModel option
    | AddPermissiveRole of RoleId
    | RemovePermissiveRole of RoleId
    | GetPermissiveRoles
    | GetUserRoles
    | RemoveUserRole of RoleId
    | UpdateRolesPermission
    | SetTemplateRole of RoleId
    | SetUserRoleToUser of RoleId * UserId

type Req =
    | Request of EventArgs.MessageCreateEventArgs * Request
    | GuildRoleDeletedHandler of EventArgs.GuildRoleDeleteEventArgs
    | ModalHandle of EventArgs.ModalSubmitEventArgs
    | ComponentInteractionCreateHandle of AsyncReplyChannel<bool> * DiscordClient * EventArgs.ComponentInteractionCreateEventArgs

type State =
    {
        GuildPermissiveRoles: PermissiveRoles.GuildPermissiveRoles
        GuildUserRoles: Roles.GuildUserRoles
        GuildTemplateRoles: TemplateRoles.GuildTemplateRoles
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

    let paddPermissiveRole: RoleId Parser =
        pstringCI "addPermissiveRole" >>. spaces
        >>. (pmentionRole <|> puint64)

    let premovePermissiveRole: RoleId Parser =
        pstringCI "removePermissiveRole" >>. spaces
        >>. (pmentionRole <|> puint64)

    let pgetPermissiveRoles: _ Parser =
        pstringCI "permissiveRoles"

    let pgetUserRoles: _ Parser =
        pstringCI "userRoles"

    let premoveUserRole: _ Parser =
        pstringCI "removeUserRole" >>. spaces
        >>. (pmentionRole <|> puint64)

    let psetTemplateRole: _ Parser =
        pstringCI "setTemplateRole" >>. spaces
        >>. (pmentionRole <|> puint64)

    let pupdateUserRolesPermissions: _ Parser =
        pstringCI "updateUserRolesPermissions" >>. spaces

    let psetUserRoleToUser: _ Parser =
        pstringCI "setUserRoleToUser" >>. spaces
        >>. tuple2
                ((pmentionRole <|> puint64) .>> spaces)
                (puserMention <|> puint64)

    let start: _ Parser =
        choice [
            pgiveOrChangeRole |>> GiveOrChangeRole
            paddPermissiveRole |>> AddPermissiveRole
            premovePermissiveRole |>> RemovePermissiveRole
            pgetPermissiveRoles >>% GetPermissiveRoles
            pgetUserRoles >>% GetUserRoles
            premoveUserRole |>> RemoveUserRole
            psetTemplateRole |>> SetTemplateRole
            pupdateUserRolesPermissions >>% UpdateRolesPermission
            psetUserRoleToUser |>> SetUserRoleToUser
        ]


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
                .WithColor(Entities.DiscordColor "#2f3136")
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

    let componentInteractionCreateHandle getRoleByUser (client: DiscordClient) (e: EventArgs.ComponentInteractionCreateEventArgs) =
        if e.Message.Author.Id = client.CurrentUser.Id then
            if e.Id.StartsWith GiveOrChangeRoleButtonId then
                let ownerId = uint64 e.Id.[GiveOrChangeRoleButtonId.Length..]
                if e.User.Id = ownerId then
                    let existRole: RoleEditModel option = getRoleByUser e.User.Id

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
                            .AddComponents(
                                Entities.TextInputComponent(
                                    "Ссылка на иконку",
                                    UserRoleFormModalIconUrl,
                                    required = false,
                                    placeholder = "",
                                    style = TextInputStyle.Short,
                                    value = (match existRole with None -> "" | Some role -> match role.IconUrl with None -> "" | Some x -> x)
                                )
                            )

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

        let guildPermissiveRoles, guildUserRoles = state.GuildPermissiveRoles, state.GuildUserRoles

        let guildMember = await (guild.GetMemberAsync userId)

        match PermissiveRoles.GuildPermissiveRoles.tryFind guild.Id guildPermissiveRoles with
        | Some permissiveRoles ->
            let hasPermissiveRole =
                guildMember.Roles
                |> Seq.exists (fun role ->
                    Set.contains role.Id permissiveRoles.RoleIds)

            if hasPermissiveRole then
                let createAndGrantRole () =
                    match TemplateRoles.GuildTemplateRoles.tryFind guild.Id state.GuildTemplateRoles with
                    | Some templateRole ->
                        let templateRole = guild.GetRole templateRole.TemplateRoleId

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
                                    let role =
                                        guild.CreateRoleAsync (
                                            name = roleEditModel.Name,
                                            color = System.Nullable roleEditModel.Color,
                                            permissions = System.Nullable templateRole.Permissions
                                        )
                                        |> await

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
                                                    .WithColor(Entities.DiscordColor "#2f3136")
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

                                    let x = Roles.insert (guild.Id, guildMember.Id, role.Id)

                                    guildUserRoles
                                    |> Map.addOrModWith
                                        x.GuildId
                                        (fun () -> Map.add x.UserId x Map.empty)
                                        (fun st -> Map.add x.UserId x st)
                            else
                                setContent "The number of roles exceeds 250"

                                guildUserRoles
                    | None ->
                        setContent "The guild owner didn't set the template role"

                        guildUserRoles

                match Map.tryFind guild.Id guildUserRoles with
                | Some userRoles ->
                    match Map.tryFind guildMember.Id userRoles with
                    | Some roleData ->
                        match guild.GetRole roleData.RoleId with
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
                | None ->
                    createAndGrantRole ()

            else
                let embed =
                    Entities.DiscordEmbedBuilder()
                        .WithColor(Entities.DiscordColor "#2f3136")
                        .WithDescription(
                            sprintf "%s, you do not have these permissive roles: %s."
                                guildMember.Mention
                                (permissiveRoles.RoleIds |> Seq.map (sprintf "<@&%d>") |> String.concat ", ")
                            )
                        .Build()

                addEmbed embed

                guildUserRoles

        | None ->
            let embed =
                Entities.DiscordEmbedBuilder()
                    .WithColor(Entities.DiscordColor "#2f3136")
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
                    match FParsecUtils.runEither Parser.phexOrDecColor hexOrDecColorStr  with
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
                        .WithColor(Entities.DiscordColor "#2f3136")
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
        else
            state

let reducer =
    let addPermisiveRole
        (e:EventArgs.MessageCreateEventArgs)
        (roleId: RoleId)
        (guildPermissiveRoles: PermissiveRoles.GuildPermissiveRoles) =

        let replyMessage =
            await (e.Channel.SendMessageAsync("Processing..."))

        let guild = e.Guild
        let currentMember = getGuildMember guild e.Author

        if currentMember.Permissions &&& Permissions.ManageRoles = Permissions.ManageRoles then
            let guildPermissiveRoles =
                guildPermissiveRoles
                |> PermissiveRoles.GuildPermissiveRoles.set
                    guild.Id
                    (fun permissiveRoles ->
                        { permissiveRoles with
                            RoleIds = Set.add roleId permissiveRoles.RoleIds })

            awaiti (replyMessage.ModifyAsync(Entities.Optional("Role has been added")))

            guildPermissiveRoles
        else
            awaiti (replyMessage.ModifyAsync(Entities.Optional("You don't have permission to manage roles")))

            guildPermissiveRoles

    let removePermissiveRole
        guildId
        roleId
        onRoleHasBeenRemoved
        onRoleDoesNotExists
        onServerDoesNotHaveRoles
        (guildPermissiveRoles: PermissiveRoles.GuildPermissiveRoles) =

            match PermissiveRoles.GuildPermissiveRoles.tryFind guildId guildPermissiveRoles with
            | Some permissiveRoles ->
                if Set.contains roleId permissiveRoles.RoleIds then
                    let guildPermissiveRoles =
                        guildPermissiveRoles
                        |> PermissiveRoles.GuildPermissiveRoles.set
                            guildId
                            (fun permissiveRoles ->
                                { permissiveRoles with
                                    RoleIds = Set.remove roleId permissiveRoles.RoleIds })

                    onRoleHasBeenRemoved guildPermissiveRoles
                else
                    onRoleDoesNotExists ()
            | None ->
                onServerDoesNotHaveRoles ()

    let removePermisiveRoleCmd
        (e:EventArgs.MessageCreateEventArgs)
        (roleId: RoleId)
        (guildPermissiveRoles: PermissiveRoles.GuildPermissiveRoles) =

        let replyMessage =
            await (e.Channel.SendMessageAsync("Processing..."))

        let guild = e.Guild
        let currentMember = getGuildMember guild e.Author

        if currentMember.Permissions &&& Permissions.ManageRoles = Permissions.ManageRoles then
            guildPermissiveRoles
            |> removePermissiveRole
                guild.Id
                roleId
                (fun guildPermissiveRoles ->
                    awaiti (replyMessage.ModifyAsync(Entities.Optional("Role has been removed")))
                    guildPermissiveRoles
                )
                (fun () ->
                    awaiti (replyMessage.ModifyAsync(Entities.Optional("Role doesn't exists in DB")))
                    guildPermissiveRoles
                )
                (fun () ->
                    awaiti (replyMessage.ModifyAsync(Entities.Optional("This server doesn't yet have permissive roles")))
                    guildPermissiveRoles
                )
        else
            awaiti (replyMessage.ModifyAsync(Entities.Optional("You don't have permission to manage roles")))

            guildPermissiveRoles

    let removeUserRole
        guildId
        roleId
        onRoleHasBeenRemoved
        onRoleDoesNotExists
        onServerDoesNotHaveUserRoles
        (guildUserRoles: Roles.GuildUserRoles) =

        match Map.tryFind guildId guildUserRoles with
        | Some userRoles ->
            let userRole =
                userRoles
                |> Map.tryPick (fun _ userRole ->
                    if userRole.RoleId = roleId then
                        Some userRole
                    else
                        None
                )

            match userRole with
            | Some roleRole ->
                Roles.remove roleRole

                let guildUserRoles =
                    let userRole =
                        Map.remove roleRole.UserId userRoles
                    Map.add guildId userRole guildUserRoles

                onRoleHasBeenRemoved guildUserRoles
            | None ->
                onRoleDoesNotExists ()
        | None ->
            onServerDoesNotHaveUserRoles ()

    let reduce msg state =
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

                state.GuildPermissiveRoles
                |> removePermissiveRole
                    guildId
                    roleId
                    (fun guildPermissiveRoles ->
                        { state with GuildPermissiveRoles = guildPermissiveRoles })
                    (fun _ -> { state with GuildUserRoles = removeUserRole state.GuildUserRoles })
                    (fun _ -> { state with GuildUserRoles = removeUserRole state.GuildUserRoles })

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
            | AddPermissiveRole roleId ->
                let guildPermissiveRoles = addPermisiveRole e roleId state.GuildPermissiveRoles

                { state with GuildPermissiveRoles = guildPermissiveRoles }
            | RemovePermissiveRole roleId ->
                let guildPermissiveRoles = removePermisiveRoleCmd e roleId state.GuildPermissiveRoles

                { state with GuildPermissiveRoles = guildPermissiveRoles }
            | GetPermissiveRoles ->
                let message =
                    match PermissiveRoles.GuildPermissiveRoles.tryFind e.Guild.Id state.GuildPermissiveRoles with
                    | Some permissiveRoles ->
                        let b = Entities.DiscordMessageBuilder()
                        let embed = Entities.DiscordEmbedBuilder()
                        embed.Color <- Entities.Optional.FromValue(Entities.DiscordColor("#2f3136"))
                        embed.Description <-
                            [
                                yield "Permissive roles: "
                                yield! permissiveRoles.RoleIds |> Set.map (sprintf "* <@&%d>")
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
                awaiti (e.Channel.SendMessageAsync (message))

                state
            | GetUserRoles ->
                let message =
                    match Map.tryFind e.Guild.Id state.GuildUserRoles with
                    | Some userRoles ->
                        let b = Entities.DiscordMessageBuilder()
                        let embed = Entities.DiscordEmbedBuilder()
                        embed.Color <- Entities.Optional.FromValue(Entities.DiscordColor("#2f3136"))
                        embed.Description <-
                            [
                                yield "User roles: "
                                yield!
                                    userRoles
                                    |> Seq.map (fun (KeyValue(_, userRole)) ->
                                        sprintf "* <@!%d> — <@&%d> (%d)" userRole.UserId userRole.RoleId userRole.RoleId
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
                        let guildTemplateRoles = state.GuildTemplateRoles
                        let guildPermissiveRoles =
                            guildTemplateRoles
                            |> TemplateRoles.GuildTemplateRoles.set
                                guild.Id
                                (fun templateRoles ->
                                    { templateRoles with
                                        TemplateRoleId = templateRoleId }
                                )

                        awaiti (replyMessage.ModifyAsync(Entities.Optional("Template role has been set")))

                        guildPermissiveRoles
                    else
                        awaiti (replyMessage.ModifyAsync(Entities.Optional("You don't have permission to manage roles")))
                        state.GuildTemplateRoles

                { state with GuildTemplateRoles = templateRoles }
            | UpdateRolesPermission ->
                let replyMessage =
                    await (e.Channel.SendMessageAsync("Processing..."))

                let guild = e.Guild
                let guildMember = getGuildMember guild e.Author
                if guildMember.Permissions &&& Permissions.ManageRoles = Permissions.ManageRoles then
                    match TemplateRoles.GuildTemplateRoles.tryFind guild.Id state.GuildTemplateRoles with
                    | Some templateRole ->
                        let templateRole = guild.GetRole templateRole.TemplateRoleId

                        if isNull templateRole then
                            awaiti <| replyMessage.ModifyAsync(Entities.Optional("The guild owner installed the template role, but it has been removed"))
                        else
                            match Map.tryFind guild.Id state.GuildUserRoles with
                            | Some userRoles ->
                                userRoles
                                |> Seq.iter (fun (KeyValue(_, userRole)) ->
                                    let userRole = guild.GetRole userRole.RoleId
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
                        awaiti <| replyMessage.ModifyAsync(Entities.Optional("The guild owner didn't set the template role"))
                else
                    awaiti (replyMessage.ModifyAsync(Entities.Optional("You don't have permission to manage roles")))

                state

            | SetUserRoleToUser(roleId, userId) ->
                awaiti <| e.Channel.TriggerTypingAsync()

                let guild = e.Guild
                let guildMember = getGuildMember guild e.Author
                if (guildMember.Permissions &&& Permissions.Administrator = Permissions.Administrator)
                    || (guildMember.Permissions &&& Permissions.ManageRoles = Permissions.ManageRoles) then

                    match Map.tryFind guild.Id state.GuildUserRoles with
                    | Some userRoleDatas ->
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

                            let userRoleData =
                                match Map.tryFind userId userRoleDatas with
                                | Some userRoleData ->
                                    // TODO: confirm to replace

                                    let userRoleData =
                                        { userRoleData with
                                            RoleId = roleId }

                                    Roles.replace userRoleData

                                    userRoleData

                                | None ->
                                    Roles.insert(guild.Id, userId, roleId )


                            awaiti <| e.Channel.SendMessageAsync "User roles has been updated"

                            { state with
                                GuildUserRoles =
                                    let m = Map.add userId userRoleData userRoleDatas
                                    Map.add guild.Id m state.GuildUserRoles
                            }

                    | None ->
                        awaiti <| e.Channel.SendMessageAsync  "This server doesn't yet have user roles"

                        state
                else
                    awaiti <| e.Channel.SendMessageAsync "You don't have permission to manage roles or administrative permission"

                    state

        | ModalHandle e ->
            UserRoleForm.modalHandle e state

        | ComponentInteractionCreateHandle(r, client, e) ->
            let getRoleByUserId userId =
                Map.tryFind e.Guild.Id state.GuildUserRoles
                |> Option.bind (fun x ->
                    Map.tryFind userId x
                    |> Option.bind (fun x ->
                        match e.Guild.GetRole x.RoleId with
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
                )

            r.Reply <| UserRoleForm.componentInteractionCreateHandle getRoleByUserId client e

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

        let init =
            {
                GuildPermissiveRoles = PermissiveRoles.GuildPermissiveRoles.getAll Db.database
                GuildUserRoles = Roles.getAll ()
                GuildTemplateRoles = TemplateRoles.GuildTemplateRoles.getAll Db.database
            }
        loop init
    )

let guildRoleDeletedHandler (e: EventArgs.GuildRoleDeleteEventArgs) =
    reducer.Post(GuildRoleDeletedHandler e)

let exec e msg =
    reducer.Post(Request (e, msg))

let modalHandle (e: EventArgs.ModalSubmitEventArgs) =
    reducer.Post (ModalHandle e)

let componentInteractionCreateHandle (client: DiscordClient) (e: EventArgs.ComponentInteractionCreateEventArgs) =
    reducer.PostAndReply (fun r -> ComponentInteractionCreateHandle (r, client, e))
