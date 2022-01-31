/// An entity that welcomes newbies and give a role or says goodbye to the leavers
module Doorkeeper.Main
open FsharpMyExtension
open FsharpMyExtension.Either
open DSharpPlus

open Types
open Model

type Template =
    | Text of string
    | UserName
    | UserMention
    static member UserNameName = "userName"
    static member UserMentionName = "userMention"

    static member ToString = function
        | Text x -> x
        | UserName -> sprintf "<@%s>" Template.UserNameName
        | UserMention -> sprintf "<@%s>" Template.UserMentionName

type NewcomersRolesMsg =
    | SetNewcomersRoles of RoleId list
    | GetNewcomersRoles

type WelcomeSettingMsg =
    | SetWelcomeSetting of ChannelId * Template list

type Request =
    | NewcomersRolesReq of NewcomersRolesMsg
    | WelcomeSettingReq of WelcomeSettingMsg

module Parser =
    open FParsec

    open DiscordMessage.Parser

    type 'Result Parser = Primitives.Parser<'Result, unit>

    let pgetNewcomersRoles: _ Parser =
        skipStringCI "newcomersRoles"

    let psetNewcomersRoles: RoleId list Parser =
        skipStringCI "setNewcomersRoles" >>. spaces
        >>. many (pmentionRole <|> puint64 .>> spaces)

    let ptemplateMessage: _ Parser =
        let praw = many1Satisfy ((<>) '<')
        let pall =
            choice [
                praw |>> Template.Text
                puserMentionTargetStr Template.UserNameName >>% UserName
                puserMentionTargetStr Template.UserMentionName >>% UserMention
                pstring "<" |>> Template.Text
            ]

        many pall

    let psetWelcomeMessage: _ Parser =
        skipStringCI "setWelcomeSetting" >>. spaces
        >>. tuple2
                (pchannelMention .>> spaces)
                ptemplateMessage

    let start: _ Parser =
        let p =
            choice [
                psetNewcomersRoles |>> SetNewcomersRoles
                pgetNewcomersRoles >>% GetNewcomersRoles

            ]
        choice [
            p |>> NewcomersRolesReq
            psetWelcomeMessage |>> (SetWelcomeSetting >> WelcomeSettingReq)
        ]

type Msg =
    | Request of EventArgs.MessageCreateEventArgs * Request
    | GuildMemberAddedHandle of EventArgs.GuildMemberAddEventArgs

type State =
    {
        NewcomersRoles: NewcomersRoles.GuildNewcomersRoles
        WelcomeSetting: WelcomeSetting.GuildWelcomeSetting
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

let welcomeSettingReduce
    (e: EventArgs.MessageCreateEventArgs)
    (msg: WelcomeSettingMsg)
    (guildWelcomeSetting: WelcomeSetting.GuildWelcomeSetting): WelcomeSetting.GuildWelcomeSetting =

    match msg with
    | SetWelcomeSetting(channelId, template) ->
        let guild = e.Guild
        let currentMember = await (guild.GetMemberAsync(e.Author.Id))
        let replyMessage =
            await (e.Channel.SendMessageAsync("Processing..."))

        if (currentMember.Permissions &&& Permissions.Administrator = Permissions.Administrator) then
            let template = template |> List.map Template.ToString |> String.concat ""
            let guildWelcomeSetting: WelcomeSetting.GuildWelcomeSetting =
                match Map.tryFind e.Guild.Id guildWelcomeSetting with
                | Some welcomeSettingData ->
                    let newcomersRoles =
                        { welcomeSettingData with
                            OutputChannel = Some channelId
                            TemplateMessage =
                                Some template

                        }

                    WelcomeSetting.replace newcomersRoles

                    Map.add guild.Id newcomersRoles guildWelcomeSetting
                | None ->
                    let x = WelcomeSetting.insert (guild.Id, Some channelId, Some template)
                    Map.add guild.Id x guildWelcomeSetting

            awaiti (replyMessage.ModifyAsync(Entities.Optional("Welcome setting has been set")))

            guildWelcomeSetting
        else
            awaiti (replyMessage.ModifyAsync(Entities.Optional("You don't have permission to manage roles")))

            guildWelcomeSetting

let reduce (msg: Msg) (state: State): State =
    match msg with
    | GuildMemberAddedHandle e ->
        if not e.Member.IsBot then
            let guildId = e.Guild.Id

            match Map.tryFind guildId state.NewcomersRoles with
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

            match Map.tryFind guildId state.WelcomeSetting with
            | Some data ->
                match data.OutputChannel, data.TemplateMessage with
                | Some outputChannelId, Some templateMessage ->
                    match e.Guild.GetChannel outputChannelId with
                    | null -> ()
                    | outputChannel ->
                        FParsecUtils.runEither Parser.ptemplateMessage templateMessage
                        |> Either.map (
                            List.map (function
                                | Text x -> x
                                | UserMention -> e.Member.Mention
                                | UserName -> e.Member.Username
                            )
                            >> System.String.Concat
                        )
                        |> Either.iter (fun msg ->
                            awaiti <| outputChannel.SendMessageAsync msg
                        )
                | _ -> ()
            | None -> ()

        state

    | Request(e, cmd) ->
        match cmd with
        | NewcomersRolesReq cmd ->
            { state with
                NewcomersRoles =
                    newcomersRolesReduce e cmd state.NewcomersRoles
            }
        | WelcomeSettingReq cmd ->
            { state with
                WelcomeSetting =
                    welcomeSettingReduce e cmd state.WelcomeSetting
            }

let m =
    let init = {
        NewcomersRoles = NewcomersRoles.getAll ()
        WelcomeSetting = WelcomeSetting.getAll ()
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

let execNewcomersRolesCmd e msg =
    m.Post (Request (e, msg))

