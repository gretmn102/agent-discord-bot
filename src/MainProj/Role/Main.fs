module Role.Main
open FsharpMyExtension
open DSharpPlus

open Types
open Model


type RoleEditModel = {
    Name: string
    Color: Entities.DiscordColor
}

let permissiveRoleId = 897089894327939082UL

type Req =
    | GiveOrChangeRole of EventArgs.MessageCreateEventArgs * RoleEditModel

let reducer =
    let giveOrChangeRole (e:EventArgs.MessageCreateEventArgs) (roleEditModel: RoleEditModel) (roles: Roles) =
        let replyMessage =
            await (e.Channel.SendMessageAsync("Processing..."))
        let guild = e.Guild
        let guildMember = await (guild.GetMemberAsync(e.Author.Id))

        let hasPermissiveRole =
            guildMember.Roles
            |> Seq.exists (fun role ->
                role.Id = permissiveRoleId)
        let createAndGrantRole () =
            if guild.Roles.Count <= 250 then
                let role =
                    guild.CreateRoleAsync (
                        name = roleEditModel.Name,
                        color = System.Nullable(roleEditModel.Color)
                    )
                    |> await

                guildMember.GrantRoleAsync role
                |> fun t -> t.GetAwaiter() |> fun x -> x.GetResult()

                awaiti <| replyMessage.ModifyAsync(Entities.Optional("Grant role"))

                let x = insert (guild.Id, guildMember.Id, role.Id)

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

    MailboxProcessor.Start (fun mail ->
        let rec loop (roles: Roles) =
            async {
                let! msg = mail.Receive()
                match msg with
                | GiveOrChangeRole (e, roleEditModel) ->
                    let roles =
                        try
                            giveOrChangeRole e roleEditModel roles
                        with e ->
                            printfn "%A" e
                            roles
                    return! loop roles
            }
        loop (getAll ())
    )

let giveOrChangeRole e roleEditModel =
    reducer.Post(GiveOrChangeRole(e, roleEditModel))


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

    let pcommand: _ Parser =
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
