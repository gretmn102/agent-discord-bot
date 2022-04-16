module Doorkeeper.Invites
open FsharpMyExtension
open FsharpMyExtension.Either
open DSharpPlus

open Types

type InvitesState = Map<GuildId, Map<string, Entities.DiscordInvite>>

type State =
    {
        InvitesState: InvitesState
        Setting: Model.InvitesSetting.GuildSetting
    }

type Setting =
    {
        ChannelId: ChannelId
        Associations: (string * string) []
    }
    static member Sample =
        {
            ChannelId = 12345UL
            Associations = [|
                "invite_code1", "came from the X"
                "invite_code2", "came from the Y"
            |]
        }

type Request =
    | SetSetting of Either<string, Setting>
    | GetSetting
    | GetInvites

module Parser =
    open FParsec

    open DiscordMessage.Parser

    type 'Result Parser = Primitives.Parser<'Result, unit>

    let setSettingName = "setInviteSetting"
    let getSettingName = "getInviteSetting"

    let getInvitesName = "invites"

    let start: _ Parser =
        let psetSetting =
            skipStringCI setSettingName >>. spaces
             >>. spaces
                >>. (pcodeBlock <|> manySatisfy (fun _ -> true))
                |>> (fun str ->
                    try
                        let setting: Setting = Json.des str
                        Right setting
                    with e ->
                        Left e.Message
                )

        choice [
            psetSetting |>> SetSetting
            skipStringCI getSettingName >>% GetSetting
            skipStringCI getInvitesName >>% GetInvites
        ]

module InviteTable =
    [<Struct>]
    type SortBy = SortByUses

    [<Literal>]
    let RefreshButtonId = "InvitesRefreshButtonId"
    [<Literal>]
    let LeftArrowButtonId = "InvitesLeftArrowButtonId"
    [<Literal>]
    let PaginationButtonId = "InvitesPaginationButtonId"
    [<Literal>]
    let RightArrowButtonId = "InvitesRightArrowButtonId"

    let createTable
        (guild: Entities.DiscordGuild)
        (addComponents: Entities.DiscordComponent [] -> _)
        addEmbed
        page
        (state: Model.InvitesSetting.GuildSetting) =

        let invites = await <| guild.GetInvitesAsync()

        let getAuthor =
            let guildInvitesSetting = Map.tryFind guild.Id state
            match guildInvitesSetting with
            | Some invitesSetting ->
                let assocs = Map.ofArray invitesSetting.Associations
                fun inviteCode ->
                    Map.tryFind inviteCode assocs
            | None -> fun _ -> None

        let itemsCountPerPage = 10
        let sortBy = SortByUses

        let itemsCount = invites.Count
        let lastPageItemsCount = itemsCount % itemsCountPerPage
        let pagesCount = itemsCount / itemsCountPerPage + if lastPageItemsCount > 0 then 1 else 0

        let table =
            let sortFunction: seq<Entities.DiscordInvite> -> _ =
                match sortBy with
                | SortByUses -> Seq.sortByDescending (fun x -> x.Uses)

            let headers = [| "Автор"; "Использовано▼" |]

            let table =
                if 0 < page && page <= pagesCount then
                    let lb = (page - 1) * itemsCountPerPage
                    let ub =
                        if page = pagesCount then if lastPageItemsCount > 0 then lastPageItemsCount else itemsCountPerPage
                        else itemsCountPerPage

                    invites
                    |> sortFunction
                    |> Seq.skip lb |> Seq.take ub
                    |> Seq.mapi (fun i user ->
                        let author =
                            getAuthor user.Code
                            |> Option.defaultWith (fun () -> sprintf "<@!%d>" user.Inviter.Id)

                        [|
                            sprintf "%d %s" (lb + i + 1) author
                            string user.Uses
                        |]
                    )
                    |> Array.ofSeq
                    |> Array.transpose
                    |> Array.map (String.concat "\n")

                else
                    Array.replicate headers.Length ""

            table
            |> Array.map2 (fun header value -> header, value) headers

        let embed =
            let b =
                Entities.DiscordEmbedBuilder()
                    .WithColor(Entities.DiscordColor "#2f3136")
                    .WithTitle("Приглашения")

            let b =
                table
                |> Array.fold
                    (fun (b: Entities.DiscordEmbedBuilder) (header, value) ->
                        // System.ArgumentException: Value cannot be empty or whitespace. (Parameter 'value')
                        let value = if value = "" then "x" else value

                        b.AddField(header, value, true)
                    )
                    b

            b.Build()

        addComponents [|
            Entities.DiscordButtonComponent(
                ButtonStyle.Secondary,
                RefreshButtonId,
                "",
                emoji = Entities.DiscordComponentEmoji(Name = "🔄") // :arrows_counterclockwise:
            )

            Entities.DiscordButtonComponent(
                ButtonStyle.Secondary,
                LeftArrowButtonId,
                "",
                disabled = (page <= 1),
                emoji = Entities.DiscordComponentEmoji(Name = "⬅️")
            )
            Entities.DiscordButtonComponent(
                ButtonStyle.Secondary,
                PaginationButtonId,
                sprintf "%d/%d" page pagesCount,
                disabled = true
            )
            Entities.DiscordButtonComponent(
                ButtonStyle.Secondary,
                RightArrowButtonId,
                "",
                disabled = (page >= pagesCount),
                emoji = Entities.DiscordComponentEmoji(Name = "➡️")
            )
        |]
        |> ignore

        addEmbed embed |> ignore

    let componentInteractionCreateHandle getState (client: DiscordClient) (e: EventArgs.ComponentInteractionCreateEventArgs) =
        let getCurrentPage () =
            e.Message.Components
            |> Seq.tryPick (fun row ->
                row.Components
                |> Seq.tryPick (fun row ->
                    if row.CustomId = PaginationButtonId then
                        let paginationButton = row :?> Entities.DiscordButtonComponent
                        let page =
                            let label = paginationButton.Label
                            let slashIndex = label.IndexOf "/"
                            int label.[..slashIndex - 1]
                        Some page
                    else
                        None
                )
            )

        let update getPage =
            let currentPage =
                match getCurrentPage () with
                | Some currentPage -> currentPage

                | None -> 1

            let state = getState ()

            let b = Entities.DiscordInteractionResponseBuilder()

            createTable e.Guild b.AddComponents b.AddEmbed (getPage currentPage) state

            e.Interaction.CreateResponseAsync(InteractionResponseType.UpdateMessage, b).GetAwaiter().GetResult()


        if e.Message.Author.Id = client.CurrentUser.Id then
            match e.Id with
            | RefreshButtonId ->
                update id
                true
            | LeftArrowButtonId ->
                update (fun currentPage -> currentPage - 1)
                true
            | RightArrowButtonId ->
                update (fun currentPage -> currentPage + 1)
                true
            | _ -> false

        else
            false


let reduceRequest (e: EventArgs.MessageCreateEventArgs) (req: Request) (state: Model.InvitesSetting.GuildSetting) =
    match req with
    | SetSetting newSetting ->
        let guild = e.Guild
        let currentMember = await (guild.GetMemberAsync e.Author.Id)
        let replyMessage =
            await (e.Channel.SendMessageAsync "Processing...")

        if currentMember.Permissions &&& Permissions.Administrator = Permissions.Administrator then
            match newSetting with
            | Right newSetting ->
                let state =
                    match Map.tryFind e.Guild.Id state with
                    | Some data ->
                        let data =
                            { data with
                                OutputChannel = newSetting.ChannelId
                                Associations = newSetting.Associations
                            }

                        Model.InvitesSetting.replace data

                        Map.add guild.Id data state
                    | None ->
                        let x = Model.InvitesSetting.insert (guild.Id, newSetting.ChannelId, newSetting.Associations)
                        Map.add guild.Id x state

                awaiti (replyMessage.ModifyAsync(Entities.Optional "Setting has been set"))

                state
            | Left errMsg ->
                let msg =
                    [
                        "```"
                        errMsg
                        "```"
                    ] |> String.concat "\n"
                awaiti (replyMessage.ModifyAsync(Entities.Optional msg))

                state

        else
            awaiti (replyMessage.ModifyAsync(Entities.Optional "You don't have administrative permission"))
            state
    | GetSetting ->
        let replyMessage =
            await (e.Channel.SendMessageAsync "Processing...")

        match Map.tryFind e.Guild.Id state with
        | Some data ->
            let msg =
                let setting =
                    {
                        ChannelId = data.OutputChannel
                        Associations = data.Associations
                    }
                    |> Json.ser

                [
                    "```"
                    setting
                    "```"
                ] |> String.concat "\n"

            awaiti (replyMessage.ModifyAsync(Entities.Optional msg))
        | None ->
            awaiti (replyMessage.ModifyAsync(Entities.Optional "Settings not set yet"))

        state

    | GetInvites ->
        e.Channel.TriggerTypingAsync().GetAwaiter().GetResult()

        let b = Entities.DiscordMessageBuilder()

        InviteTable.createTable e.Guild b.AddComponents b.AddEmbed 1 state

        awaiti <| e.Channel.SendMessageAsync b

        state

type Req =
    | GuildAvailableHandle of Entities.DiscordGuild
    | InviteCreatedHandle of EventArgs.InviteCreateEventArgs
    | InviteDeletedHandle of EventArgs.InviteDeleteEventArgs
    | GuildMemberAddedHandle of EventArgs.GuildMemberAddEventArgs
    | Request of EventArgs.MessageCreateEventArgs * Request
    | GetState of AsyncReplyChannel<Model.InvitesSetting.GuildSetting>

let reduce (req: Req) (state: State) =
    match req with
    | GuildMemberAddedHandle e ->
        match Map.tryFind e.Guild.Id state.Setting with
        | Some setting ->
            match Map.tryFind e.Guild.Id state.InvitesState with
            | Some oldInvites ->
                let newInvites = await (e.Guild.GetInvitesAsync())
                let diff =
                    newInvites
                    |> Seq.choose (fun newInvite ->
                        match Map.tryFind newInvite.Code oldInvites with
                        | Some oldInvite ->
                            if newInvite.Uses <> oldInvite.Uses then
                                Some newInvite
                            else None
                        | None ->
                            Some newInvite
                    )
                    |> List.ofSeq

                match diff with
                | [ invite ] ->
                    match e.Guild.GetChannel setting.OutputChannel with
                    | null -> state
                    | channel ->
                        let embed = Entities.DiscordEmbedBuilder()
                        embed.Description <-
                            match Array.tryFind (fun (code, _) -> code = invite.Code) setting.Associations with
                            | None ->
                                sprintf "%s пришел на сервер по [ссылке](https://discord.gg/%s), которую создал %s." e.Member.Mention invite.Code invite.Inviter.Mention
                            | Some (_, message) ->
                                sprintf "%s %s" e.Member.Mention message

                        let b = Entities.DiscordMessageBuilder()
                        b.Embed <- embed.Build ()

                        try
                            awaiti (channel.SendMessageAsync b)
                        with _ -> ()

                        { state with
                            InvitesState =
                                let invites = Map.add invite.Code invite oldInvites
                                Map.add e.Guild.Id invites state.InvitesState }

                | invites ->
                    printfn "Something went wrong:\n%A" invites

                    { state with
                        InvitesState =
                            let invites =
                                invites
                                |> List.fold
                                    (fun oldInvites invite -> Map.add invite.Code invite oldInvites)
                                    oldInvites
                            Map.add e.Guild.Id invites state.InvitesState }

            | None ->
                printfn "Something went wrong"

                state

        | None -> state

    | InviteCreatedHandle e ->
        let inviteCreatedHandle (e: EventArgs.InviteCreateEventArgs) (state: InvitesState) =
            let invites =
                match Map.tryFind e.Guild.Id state with
                | Some invites -> invites
                | None -> Map.empty

            let invites = Map.add e.Invite.Code e.Invite invites
            Map.add e.Guild.Id invites state

        { state with
            InvitesState =
                inviteCreatedHandle e state.InvitesState
        }

    | InviteDeletedHandle e ->
        let inviteDeletedHandle (e: EventArgs.InviteDeleteEventArgs) (state: InvitesState) =
            let invites =
                match Map.tryFind e.Guild.Id state with
                | Some invites -> Map.remove e.Invite.Code invites
                | None -> Map.empty

            Map.add e.Guild.Id invites state

        { state with
            InvitesState =
                inviteDeletedHandle e state.InvitesState
        }

    | GuildAvailableHandle guild ->
        let guildAvailableHandle (guild: Entities.DiscordGuild) (state: InvitesState) =
            let invites =
                try
                    await (guild.GetInvitesAsync())
                with e ->
                    [] :> System.Collections.Generic.IReadOnlyList<_>

            let guildInvites =
                invites
                |> Seq.fold
                    (fun state invite ->
                        Map.add invite.Code invite state
                    )
                    Map.empty

            Map.add guild.Id guildInvites state

        { state with
            InvitesState =
                guildAvailableHandle guild state.InvitesState
        }

    | Request (e, r) ->
        { state with
            Setting =
                reduceRequest e r state.Setting
        }

    | GetState r ->
        r.Reply state.Setting

        state

let m =
    let init = {
        InvitesState = Map.empty
        Setting = Model.InvitesSetting.getAll ()
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

let guildMemberAddedHandle guild =
    m.Post (GuildMemberAddedHandle guild)
let inviteCreatedHandle e =
    m.Post (InviteCreatedHandle e)
let inviteDeletedHandle e =
    m.Post (InviteDeletedHandle e)
let guildAvailableHandle e =
    m.Post (GuildAvailableHandle e)
let exec e r =
    m.Post (Request(e, r))
let componentInteractionCreateHandle (client: DiscordClient) (e: EventArgs.ComponentInteractionCreateEventArgs) =
    InviteTable.componentInteractionCreateHandle
        (fun () -> m.PostAndReply GetState)
        client
        e
