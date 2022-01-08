/// An entity that welcomes newbies and give a role or says goodbye to the leavers
module Doorkeeper.Main
open DSharpPlus

open Types
open Model

type NewcomersRolesMsg =
    | SetNewcomersRoles of RoleId list
    | GetNewcomersRoles

type Msg =
    | NewcomersRolesCmd of EventArgs.MessageCreateEventArgs * NewcomersRolesMsg
    | GuildMemberAddedHandle of EventArgs.GuildMemberAddEventArgs

type State =
    {
        NewcomersRoles: NewcomersRoles.GuildNewcomersRoles
    }

let newcomersRolesReduce
    (e: EventArgs.MessageCreateEventArgs)
    (msg: NewcomersRolesMsg)
    (guildNewcomersRoles: NewcomersRoles.GuildNewcomersRoles): NewcomersRoles.GuildNewcomersRoles =

    match msg with
    | SetNewcomersRoles roleIds ->
        let guild = e.Guild
        let currentMember = await (guild.GetMemberAsync(e.Author.Id))
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
                    let x = NewcomersRoles.insert (guild.Id, Set.ofList roleIds)
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

let reduce (msg: Msg) (state: State): State =
    match msg with
    | GuildMemberAddedHandle e ->
        match Map.tryFind e.Guild.Id state.NewcomersRoles with
        | Some data ->
            data.RoleIds
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
        | None -> ()

        state

    | NewcomersRolesCmd (e, cmd) ->
        { state with
            NewcomersRoles =
                newcomersRolesReduce e cmd state.NewcomersRoles
        }

let m =
    let init = {
        NewcomersRoles = NewcomersRoles.getAll ()
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

let handle (e: EventArgs.GuildMemberAddEventArgs) =
    m.Post (GuildMemberAddedHandle e)

module Parser =
    open FParsec
    type 'Result Parser = Primitives.Parser<'Result, unit>

    let pmentionRole: RoleId Parser =
        skipString "<@&" >>. puint64 .>> skipChar '>'
    let pmentionRoleTarget (roleId: RoleId) : _ Parser =
        skipString "<@&" >>. skipString (string roleId) .>> skipChar '>'

    let pgetNewcomersRoles: _ Parser =
        skipStringCI "newcomersRoles"

    let psetNewcomersRoles: RoleId list Parser =
        skipStringCI "setNewcomersRoles" >>. spaces
        >>. many (pmentionRole <|> puint64 .>> spaces)

    let start: _ Parser =
        choice [
            psetNewcomersRoles |>> fun roleIds (e: EventArgs.MessageCreateEventArgs) ->
                m.Post (NewcomersRolesCmd (e, SetNewcomersRoles roleIds))

            pgetNewcomersRoles >>% fun (e: EventArgs.MessageCreateEventArgs) ->
                m.Post (NewcomersRolesCmd (e, GetNewcomersRoles))
        ]
