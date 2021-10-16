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
    let removePermisiveRole
        (e:EventArgs.MessageCreateEventArgs)
        (roleId: RoleId)
        (guildPermissiveRoles: PermissiveRoles.GuildPermissiveRoles) =

        let replyMessage =
            await (e.Channel.SendMessageAsync("Processing..."))

        let guild = e.Guild
        let currentMember = await (guild.GetMemberAsync(e.Author.Id))

        if currentMember.Permissions &&& Permissions.ManageRoles = Permissions.ManageRoles then
            match Map.tryFind guild.Id guildPermissiveRoles with
            | Some permissiveRoles ->
                if Set.contains roleId permissiveRoles.RoleIds then
                    let permissiveRoles =
                        { permissiveRoles with
                            RoleIds = Set.remove roleId permissiveRoles.RoleIds }

                    PermissiveRoles.replace permissiveRoles

                    let guildPermissiveRoles = Map.add guild.Id permissiveRoles guildPermissiveRoles

                    awaiti (replyMessage.ModifyAsync(Entities.Optional("Role has been removed")))
                    guildPermissiveRoles
                else
                    awaiti (replyMessage.ModifyAsync(Entities.Optional("The role has already been removed")))

                    guildPermissiveRoles

            | None ->
                awaiti (replyMessage.ModifyAsync(Entities.Optional("The role has already been removed")))

                guildPermissiveRoles
        else
            awaiti (replyMessage.ModifyAsync(Entities.Optional("You don't have permission to manage roles")))

            guildPermissiveRoles

    MailboxProcessor.Start (fun mail ->
        let rec loop (guildPermissiveRoles: PermissiveRoles.GuildPermissiveRoles, guildUserRoles: Roles.GuildUserRoles) =
            async {
                let! msg = mail.Receive()
                match msg with
                | GiveOrChangeRole (e, roleEditModel) ->
                    let roles =
                        try
                            giveOrChangeRole e roleEditModel guildPermissiveRoles guildUserRoles
                        with e ->
                            printfn "%A" e
                            guildUserRoles

                    return! loop (guildPermissiveRoles, roles)
                | AddPermissiveRole (e, roleId) ->
                    let guildPermissiveRoles = addPermisiveRole e roleId guildPermissiveRoles

                    return! loop (guildPermissiveRoles, guildUserRoles)
                | RemovePermissiveRole (e, roleId) ->
                    let guildPermissiveRoles = removePermisiveRole e roleId guildPermissiveRoles

                    return! loop (guildPermissiveRoles, guildUserRoles)
                | GetPermissiveRoles e ->
                    let message =
                        match Map.tryFind e.Guild.Id guildPermissiveRoles with
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

                    return! loop (guildPermissiveRoles, guildUserRoles)
                | GetUserRoles e ->
                    let message =
                        match Map.tryFind e.Guild.Id guildUserRoles with
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
                                            sprintf "* <@!%d> — <@&%d>" userRole.UserId userRole.RoleId
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

                    return! loop (guildPermissiveRoles, guildUserRoles)
            }

        loop (PermissiveRoles.getAll (), Roles.getAll ())
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
