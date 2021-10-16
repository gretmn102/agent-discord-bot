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

let reducer =
    let giveOrChangeRole
        (e:EventArgs.MessageCreateEventArgs)
        (roleEditModel: RoleEditModel)
        (guildPermissiveRoles: PermissiveRoles.GuildPermissiveRoles)
        (roles: Roles.Roles) =

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

                Map.add (guild.Id, guildMember.Id) x roles
            else
                awaiti <| replyMessage.ModifyAsync(Entities.Optional("The number of roles exceeds 250"))

                roles

        if hasPermissiveRole then
            match Map.tryFind (guild.Id, guildMember.Id) roles with
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

                    roles
            | None ->
                createAndGrantRole ()
        else
            awaiti <| replyMessage.ModifyAsync(Entities.Optional("You don't have permissive role"))

            roles

    let addPermisiveRole
        (e:EventArgs.MessageCreateEventArgs)
        (roleId: RoleId)
        (guildPermissiveRoles: PermissiveRoles.GuildPermissiveRoles) =

        let replyMessage =
            await (e.Channel.SendMessageAsync("Processing..."))

        let guild = e.Guild

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

    let removePermisiveRole
        (e:EventArgs.MessageCreateEventArgs)
        (roleId: RoleId)
        (guildPermissiveRoles: PermissiveRoles.GuildPermissiveRoles) =

        let replyMessage =
            await (e.Channel.SendMessageAsync("Processing..."))

        let guild = e.Guild
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

    MailboxProcessor.Start (fun mail ->
        let rec loop (guildPermissiveRoles: PermissiveRoles.GuildPermissiveRoles, roles: Roles.Roles) =
            async {
                let! msg = mail.Receive()
                match msg with
                | GiveOrChangeRole (e, roleEditModel) ->
                    let roles =
                        try
                            giveOrChangeRole e roleEditModel guildPermissiveRoles roles
                        with e ->
                            printfn "%A" e
                            roles

                    return! loop (guildPermissiveRoles, roles)
                | AddPermissiveRole (e, roleId) ->
                    let guildPermissiveRoles = addPermisiveRole e roleId guildPermissiveRoles

                    return! loop (guildPermissiveRoles, roles)
                | RemovePermissiveRole (e, roleId) ->
                    let guildPermissiveRoles = removePermisiveRole e roleId guildPermissiveRoles

                    return! loop (guildPermissiveRoles, roles)
            }

        loop (PermissiveRoles.getAll (), Roles.getAll ())
    )

let giveOrChangeRole e roleEditModel =
    reducer.Post(GiveOrChangeRole(e, roleEditModel))

let addPermisiveRole e roleId =
    reducer.Post(AddPermissiveRole(e, roleId))

let removePermisiveRole e roleId =
    reducer.Post(RemovePermissiveRole(e, roleId))

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
