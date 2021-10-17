module Role.Main
open FsharpMyExtension
open DSharpPlus

open Types
open Model


type RoleEditModel = {
    Name: string
    Color: Entities.DiscordColor
}

type Req =
    | GiveOrChangeRole of EventArgs.MessageCreateEventArgs * RoleEditModel
    | AddPermissiveRole of EventArgs.MessageCreateEventArgs * RoleId
    | RemovePermissiveRole of EventArgs.MessageCreateEventArgs * RoleId
    | GetPermissiveRoles of EventArgs.MessageCreateEventArgs
    | GetUserRoles of EventArgs.MessageCreateEventArgs
    | RemoveUserRole of EventArgs.MessageCreateEventArgs * RoleId
    | GuildRoleDeletedHandler of EventArgs.GuildRoleDeleteEventArgs

    | SetTemplateRole of EventArgs.MessageCreateEventArgs * RoleId

type State =
    {
        GuildPermissiveRoles: PermissiveRoles.GuildPermissiveRoles
        GuildUserRoles: Roles.GuildUserRoles
        GuildTemplateRoles: TemplateRoles.GuildTemplateRoles
    }

let reducer =
    let giveOrChangeRole
        (e:EventArgs.MessageCreateEventArgs)
        (roleEditModel: RoleEditModel)
        (guildPermissiveRoles: PermissiveRoles.GuildPermissiveRoles)
        (guildUserRoles: Roles.GuildUserRoles) =

        let replyMessage =
            await (e.Channel.SendMessageAsync("Processing..."))
        let guild = e.Guild
        let guildMember = await (guild.GetMemberAsync(e.Author.Id))

        let hasPermissiveRole =
            match Map.tryFind guild.Id guildPermissiveRoles with
            | Some permissiveRoles ->
                guildMember.Roles
                |> Seq.exists (fun role ->
                    Set.contains role.Id permissiveRoles.RoleIds)
            | None -> false

        let createAndGrantRole () =
            if guild.Roles.Count < 250 then
                let role =
                    guild.CreateRoleAsync (
                        name = roleEditModel.Name,
                        color = System.Nullable(roleEditModel.Color)
                    )
                    |> await

                guildMember.GrantRoleAsync role
                |> fun t -> t.GetAwaiter() |> fun x -> x.GetResult()

                awaiti <| replyMessage.ModifyAsync(Entities.Optional("Role granted"))

                let x = Roles.insert (guild.Id, guildMember.Id, role.Id)

                guildUserRoles
                |> Map.addOrModWith
                    x.GuildId
                    (fun () -> Map.add x.UserId x Map.empty)
                    (fun st -> Map.add x.UserId x st)
            else
                awaiti <| replyMessage.ModifyAsync(Entities.Optional("The number of roles exceeds 250"))

                guildUserRoles

        if hasPermissiveRole then
            match Map.tryFind guild.Id guildUserRoles with
            | Some userRoles ->
                match Map.tryFind guildMember.Id userRoles with
                | Some roleData ->
                    let userRole = guild.GetRole(roleData.RoleId)
                    if isNull userRole then
                        createAndGrantRole ()
                    else
                        userRole.ModifyAsync (
                            name = roleEditModel.Name,
                            color = System.Nullable(roleEditModel.Color)
                        )
                        |> fun t -> t.GetAwaiter() |> fun x -> x.GetResult()

                        awaiti <| replyMessage.ModifyAsync(Entities.Optional("Changed role"))

                        guildUserRoles
                | None ->
                    createAndGrantRole ()
            | None ->
                createAndGrantRole ()
        else
            awaiti <| replyMessage.ModifyAsync(Entities.Optional("You don't have permissive role"))

            guildUserRoles

    let addPermisiveRole
        (e:EventArgs.MessageCreateEventArgs)
        (roleId: RoleId)
        (guildPermissiveRoles: PermissiveRoles.GuildPermissiveRoles) =

        let replyMessage =
            await (e.Channel.SendMessageAsync("Processing..."))

        let guild = e.Guild
        let currentMember = await (guild.GetMemberAsync(e.Author.Id))

        if currentMember.Permissions &&& Permissions.ManageRoles = Permissions.ManageRoles then
            let guildPermissiveRoles =
                match Map.tryFind guild.Id guildPermissiveRoles with
                | Some permissiveRoles ->
                    let permissiveRoles =
                        { permissiveRoles with
                            RoleIds = Set.add roleId permissiveRoles.RoleIds }

                    PermissiveRoles.replace permissiveRoles

                    Map.add guild.Id permissiveRoles guildPermissiveRoles
                | None ->
                    let x = PermissiveRoles.insert (guild.Id, Set.singleton roleId)
                    Map.add guild.Id x guildPermissiveRoles

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

            match Map.tryFind guildId guildPermissiveRoles with
            | Some permissiveRoles ->
                if Set.contains roleId permissiveRoles.RoleIds then
                    let permissiveRoles =
                        { permissiveRoles with
                            RoleIds = Set.remove roleId permissiveRoles.RoleIds }

                    PermissiveRoles.replace permissiveRoles

                    let guildPermissiveRoles = Map.add guildId permissiveRoles guildPermissiveRoles

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
        let currentMember = await (guild.GetMemberAsync(e.Author.Id))

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

    MailboxProcessor.Start (fun mail ->
        let rec loop (state: State) =
            async {
                let! msg = mail.Receive()
                match msg with
                | GiveOrChangeRole (e, roleEditModel) ->
                    let roles =
                        try
                            giveOrChangeRole e roleEditModel state.GuildPermissiveRoles state.GuildUserRoles
                        with e ->
                            printfn "%A" e
                            state.GuildUserRoles

                    return! loop { state with GuildUserRoles = roles }
                | AddPermissiveRole (e, roleId) ->
                    let guildPermissiveRoles = addPermisiveRole e roleId state.GuildPermissiveRoles

                    return! loop { state with GuildPermissiveRoles = guildPermissiveRoles }
                | RemovePermissiveRole (e, roleId) ->
                    let guildPermissiveRoles = removePermisiveRoleCmd e roleId state.GuildPermissiveRoles

                    return! loop { state with GuildPermissiveRoles = guildPermissiveRoles }
                | GetPermissiveRoles e ->
                    let message =
                        match Map.tryFind e.Guild.Id state.GuildPermissiveRoles with
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

                    return! loop state
                | GetUserRoles e ->
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

                    return! loop state
                | RemoveUserRole (e, userRoleId) ->
                    let replyMessage =
                        await (e.Channel.SendMessageAsync("Processing..."))

                    let guild = e.Guild
                    let guildMember = await (guild.GetMemberAsync(e.Author.Id))
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

                    return! loop { state with GuildUserRoles = guildUserRoles }
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

                    return! loop state
                | SetTemplateRole (e, templateRoleId) ->
                    let replyMessage =
                        await (e.Channel.SendMessageAsync("Processing..."))

                    let guild = e.Guild
                    let guildMember = await (guild.GetMemberAsync(e.Author.Id))
                    let templateRoles =
                        if guildMember.Permissions &&& Permissions.ManageRoles = Permissions.ManageRoles then
                            let guildTemplateRoles = state.GuildTemplateRoles
                            let guildPermissiveRoles =
                                match Map.tryFind guild.Id guildTemplateRoles with
                                | Some templateRoles ->
                                    let templateRoles =
                                        { templateRoles with
                                            TemplateRoleId = templateRoleId }

                                    TemplateRoles.replace templateRoles

                                    Map.add guild.Id templateRoles guildTemplateRoles
                                | None ->
                                    let x = TemplateRoles.insert (guild.Id, templateRoleId)
                                    Map.add guild.Id x guildTemplateRoles

                            awaiti (replyMessage.ModifyAsync(Entities.Optional("Template role has been set")))

                            guildPermissiveRoles
                        else
                            awaiti (replyMessage.ModifyAsync(Entities.Optional("You don't have permission to manage roles")))
                            state.GuildTemplateRoles

                    return! loop { state with GuildTemplateRoles = templateRoles }
            }

        let init =
            {
                GuildPermissiveRoles = PermissiveRoles.getAll ()
                GuildUserRoles = Roles.getAll ()
                GuildTemplateRoles = TemplateRoles.getAll ()
            }
        loop init
    )

let giveOrChangeRole e roleEditModel =
    reducer.Post(GiveOrChangeRole(e, roleEditModel))

let addPermisiveRole e roleId =
    reducer.Post(AddPermissiveRole(e, roleId))

let removePermisiveRole e roleId =
    reducer.Post(RemovePermissiveRole(e, roleId))

let getPermisiveRole e =
    reducer.Post(GetPermissiveRoles e)

let getUserRoles e =
    reducer.Post(GetUserRoles e)

let removeUserRole e roleId =
    reducer.Post(RemoveUserRole(e, roleId))

let guildRoleDeletedHandler (e: EventArgs.GuildRoleDeleteEventArgs) =
    reducer.Post(GuildRoleDeletedHandler e)

let setTemplateRole e roleId =
    reducer.Post(SetTemplateRole(e, roleId))

module Parser =
    open FParsec
    type 'Result Parser = Primitives.Parser<'Result, unit>

    let pquote: _ Parser =
        between
            (skipChar '"')
            (skipChar '"')
            (many1Strings (
                many1Satisfy (isNoneOf "\"\\")
                <|> (skipChar '\\' >>. ((pchar '"' |>> string) <|>% "\\"))
            ))

    let phexColor: _ Parser =
        pchar '#'
        >>. manyMinMaxSatisfy 1 6 isHex
        |>> fun hex ->
            System.Convert.ToInt32(hex, 16)
            |> Entities.DiscordColor

    let pmentionRole: RoleId Parser =
        skipString "<@&" >>. puint64 .>> skipChar '>'
    let pmentionRoleTarget (roleId: RoleId) : _ Parser =
        skipString "<@&" >>. skipString (string roleId) .>> skipChar '>'

    let pgiveOrChangeRole: _ Parser =
        pstringCI "role" >>. spaces
        >>. pipe2
            (pquote .>> spaces)
            phexColor
            (fun name color ->
                {
                    Name = name
                    Color = color
                }
            )

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
