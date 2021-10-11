module Role.Main
open FsharpMyExtension
open DSharpPlus

open Types


type RoleEditModel = {
    Name: string
    Color: Entities.DiscordColor
}

type Roles = Map<GuildId * UserId, RoleId>

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

        if hasPermissiveRole then
            match Map.tryFind (guild.Id, guildMember.Id) roles with
            | Some userRoleId ->
                let userRole = guild.GetRole(userRoleId)

                userRole.ModifyAsync (
                    name = roleEditModel.Name,
                    color = System.Nullable(roleEditModel.Color)
                )
                |> fun t -> t.GetAwaiter() |> fun x -> x.GetResult()

                awaiti <| replyMessage.ModifyAsync(Entities.Optional("Changed role"))

                roles
            | None ->
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

                    // TODO: write to db
                    Map.add (guild.Id, guildMember.Id) role.Id roles
                else
                    awaiti <| replyMessage.ModifyAsync(Entities.Optional("The number of roles exceeds 250"))

                    roles
        else
            awaiti <| replyMessage.ModifyAsync(Entities.Optional("you don't have permissive role"))

            roles

    MailboxProcessor.Start (fun mail ->
        let rec loop (roles: Roles) =
            async {
                let! msg = mail.Receive()
                match msg with
                | GiveOrChangeRole (e, roleEditModel) ->
                let roles = giveOrChangeRole e roleEditModel roles

                return! loop roles
            }
        loop Map.empty
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
