module Ranking.Main
open FsharpMyExtension
open DSharpPlus

open Types
open Model

type RankingSettingRequest =
    | SetOutputChannel of ChannelId
    | SetLevelRoles of (ChannelId * int) list
    | GetLevelRoles

type MostActiveSettingRequest =
    | SetMostActiveRole of RoleId
    | GetMostActiveRole

type Request =
    | SetExp of UserId * uint64
    | GetRank of UserId option
    | SettingRequest of RankingSettingRequest

    | MostActiveSettingRequest of MostActiveSettingRequest
    | MostActiveActivate
    | MostActiveLeaderboard

type Ticks = int64

type State =
    {
        Settings: RankingSettings.GuildRankingSettings
        Rankings: Rankings.GuildRankings
        CoolDowns: Map<UserId, Ticks>
        MostActiveSettings: MostActiveSettings.GuildDatas
    }

type Msg =
    | NewMessageHandle of EventArgs.MessageCreateEventArgs
    | Request of EventArgs.MessageCreateEventArgs * Request
    | MostActiveActivateAll of DiscordClient
    | GetState of AsyncReplyChannel<State>

let lowerBoundExp, upperBoundExp = 15, 25

let getExpByLevel level =
    let level = uint64 level
    5UL * (level * level) + (50UL * level) + 100UL

let getLevelTotalExp level =
    let rec f currExp currentLevel =
        if currentLevel < level then
            let exp = getExpByLevel(currentLevel)
            f (currExp + exp) (currentLevel + 1UL)
        else
            currExp
    f 0UL 0UL

let getLevelBySumExp exp =
    let rec f currExp level =
        let exp = getExpByLevel(level)
        if currExp >= exp then
            f (currExp - exp) (level + 1UL)
        else
            level
    f exp 0UL

let coolDown: Ticks = 10000L * 1000L * 60L // 60 sec

let r = System.Random ()

let updateExp
    (e: EventArgs.MessageCreateEventArgs)
    (userId: UserId)
    cooldown
    (setting: RankingSettings.RankingSettingData)
    update
    (state: State) =

    let exec rankings =
        let exec (rank: Rankings.RankingData) (state: State) =
            let oldLevel = getLevelBySumExp rank.Exp
            let newExp = update rank.Exp
            let newLevel = getLevelBySumExp newExp

            if newLevel > 0UL && oldLevel <> newLevel then
                let foundRole =
                    setting.LevelRoles
                    |> Array.tryPick (fun levelRole ->
                        if levelRole.Level = int newLevel then
                            match e.Guild.Roles.[levelRole.Role] with
                            | null -> None
                            | role -> Some role
                        else
                            None
                    )

                match foundRole with
                | None -> ()
                | Some role ->
                    try
                        let guildMember = await (e.Guild.GetMemberAsync userId)

                        guildMember.GrantRoleAsync(role)
                        |> fun x -> x.GetAwaiter() |> fun x -> x.GetResult()

                        guildMember.Roles
                        |> Array.ofSeq // because RevokeRoleAsync may invoke "Collection was modified; enumeration operation may not execute."
                        |> Seq.iter (fun currRole ->
                            let currRoleId = currRole.Id
                            if currRoleId <> role.Id
                               && setting.LevelRoles |> Array.exists (fun x -> currRole.Id = x.Role)

                            then
                                try
                                    guildMember.RevokeRoleAsync currRole
                                    |> fun x -> x.GetAwaiter() |> fun x -> x.GetResult()
                                with e ->
                                    printfn "2 %A" e.Message
                        )
                    with e ->
                        printfn "1 %A" e.Message

                match setting.OutputChannelId with
                | Some outputChannelId ->
                    match e.Guild.GetChannel outputChannelId with
                    | null -> ()
                    | outputChannel ->
                        let msg =
                            match foundRole with
                            | None ->
                                sprintf "<@!%d> поднялся до %d уровня." e.Author.Id newLevel
                            | Some role ->
                                sprintf "<@!%d> поднялся до %d уровня и получил роль <@&%d>." e.Author.Id newLevel role.Id

                        let b = Entities.DiscordMessageBuilder()

                        let color =
                            match foundRole with
                            | None -> Entities.DiscordColor "#2f3136"
                            | Some x -> x.Color

                        let embed =
                            Entities.DiscordEmbedBuilder()
                                .WithDescription(msg)
                                .WithColor(color)
                                .Build()

                        b.WithEmbed embed |> ignore

                        try
                            awaiti (outputChannel.SendMessageAsync(b))
                        with e ->
                            printfn "I don't have the permission to write to %d channel" outputChannel.Id

                | None -> ()

            let newRank =
                { rank with
                    Exp = newExp
                    DayExp = (newExp - rank.Exp) + rank.DayExp
                }

            Rankings.replace newRank

            { state with
                Rankings =
                    let m = Map.add userId newRank rankings
                    Map.add e.Guild.Id m state.Rankings
            }

        match Map.tryFind userId rankings with
        | Some rank ->
            match Map.tryFind userId state.CoolDowns with
            | Some lastTime ->
                let now = System.DateTime.Now.Ticks

                if now - lastTime - cooldown < 0L then
                    state
                else
                    let state =
                        { state with
                            CoolDowns = Map.add userId now state.CoolDowns
                        }

                    exec rank state
            | None ->
                let now = System.DateTime.Now.Ticks
                let state =
                    { state with
                        CoolDowns = Map.add userId now state.CoolDowns
                    }

                exec rank state
        | None ->
            let rank = Rankings.insert(e.Guild.Id, userId, 0UL)

            let now = System.DateTime.Now.Ticks
            let state =
                { state with
                    CoolDowns = Map.add userId now state.CoolDowns
                }

            exec rank state

    match Map.tryFind e.Guild.Id state.Rankings with
    | Some rankings ->
        exec rankings
    | None ->
        exec Map.empty

let rankingSettingReduce
    (e: EventArgs.MessageCreateEventArgs)
    (msg: RankingSettingRequest)
    (state: State): State =

    match msg with
    | SetOutputChannel outputChannelId ->
        let guild = e.Guild
        let currentMember = await (guild.GetMemberAsync(e.Author.Id))
        let replyMessage =
            await (e.Channel.SendMessageAsync("Processing..."))

        if currentMember.Permissions &&& Permissions.Administrator = Permissions.Administrator then
            let settings: RankingSettings.GuildRankingSettings =
                let settings = state.Settings

                match Map.tryFind e.Guild.Id settings with
                | Some setting ->
                    let newcomersRoles =
                        { setting with
                            OutputChannelId = Some outputChannelId
                        }

                    RankingSettings.replace newcomersRoles

                    Map.add guild.Id newcomersRoles settings
                | None ->
                    let x = RankingSettings.insert (guild.Id, [||], Some outputChannelId)
                    Map.add guild.Id x settings

            awaiti (replyMessage.ModifyAsync(Entities.Optional("Output channel for ranking has been set")))

            { state with Settings = settings}
        else
            awaiti (replyMessage.ModifyAsync(Entities.Optional("You don't have permission")))

            state

    | SetLevelRoles levelRoles ->
        let guild = e.Guild
        let currentMember = await (guild.GetMemberAsync(e.Author.Id))
        let replyMessage =
            await (e.Channel.SendMessageAsync("Processing..."))

        if currentMember.Permissions &&& Permissions.Administrator = Permissions.Administrator then
            let levelRoles =
                List.sort levelRoles
                |> List.map (fun (roleId, level) ->
                    { Role = roleId; Level = level } : RankingSettings.LevelRole
                )
                |> Array.ofList

            let settings: RankingSettings.GuildRankingSettings =
                let settings = state.Settings

                match Map.tryFind e.Guild.Id settings with
                | Some setting ->
                    let setting =
                        { setting with
                            LevelRoles = levelRoles
                        }

                    RankingSettings.replace setting

                    Map.add guild.Id setting settings
                | None ->
                    let x = RankingSettings.insert (guild.Id, levelRoles, None)
                    Map.add guild.Id x settings

            awaiti (replyMessage.ModifyAsync(Entities.Optional("Roles level has been set")))

            { state with Settings = settings}

        else
            awaiti (replyMessage.ModifyAsync(Entities.Optional("You don't have permission")))

            state

    | GetLevelRoles ->
        let guild = e.Guild
        let replyMessage =
            await (e.Channel.SendMessageAsync("Processing..."))

        match Map.tryFind guild.Id state.Settings with
        | Some setting ->
            let msg =
                setting.LevelRoles
                |> Array.map (fun x -> sprintf "<@&%d> — %d" x.Role x.Level)
                |> String.concat "\n"

            let b = Entities.DiscordMessageBuilder()

            let color = Entities.DiscordColor "#2f3136"

            let embed =
                Entities.DiscordEmbedBuilder()
                    .WithDescription(msg)
                    .WithColor(color)
                    .Build()

            b.WithEmbed embed |> ignore

            awaiti (replyMessage.ModifyAsync(b))
        | None ->
            awaiti (replyMessage.ModifyAsync(Entities.Optional "Firstly set up the ranking system"))

        state

let mostActiveSettingReduce
    (e: EventArgs.MessageCreateEventArgs)
    (msg: MostActiveSettingRequest)
    (state: State): State =

    match msg with
    | SetMostActiveRole roleId ->
        let guild = e.Guild
        let currentMember = await (guild.GetMemberAsync(e.Author.Id))
        let replyMessage =
            await (e.Channel.SendMessageAsync "Processing...")

        if currentMember.Permissions &&& Permissions.Administrator = Permissions.Administrator then
            let settings: MostActiveSettings.GuildDatas =
                let settings = state.MostActiveSettings

                match Map.tryFind e.Guild.Id settings with
                | Some setting ->
                    let setting =
                        { setting with
                            MostActiveRoleId = roleId
                        }

                    MostActiveSettings.replace setting

                    Map.add guild.Id setting settings
                | None ->
                    let x = MostActiveSettings.insert (guild.Id, roleId, 0UL, System.DateTime.UtcNow)
                    Map.add guild.Id x settings

            awaiti (replyMessage.ModifyAsync(Entities.Optional("Most active role has been set")))

            { state with MostActiveSettings = settings }

        else
            awaiti (replyMessage.ModifyAsync(Entities.Optional("You don't have administrative permission")))

            state


    | GetMostActiveRole ->
        let guild = e.Guild
        let replyMessage =
            await (e.Channel.SendMessageAsync "Processing...")

        match Map.tryFind guild.Id state.MostActiveSettings with
        | Some setting ->
            let b = Entities.DiscordMessageBuilder()

            let embed =
                Entities.DiscordEmbedBuilder()
                    .WithDescription(sprintf "<@&%d>" setting.MostActiveRoleId)
                    .WithColor(Entities.DiscordColor "#2f3136")
                    .Build()

            b.WithEmbed embed |> ignore

            awaiti (replyMessage.ModifyAsync(b))
        | None ->
            awaiti (replyMessage.ModifyAsync(Entities.Optional "A most active role is not set"))

        state

let mostActiveActivate (guild: Entities.DiscordGuild) (mostActiveSetting: MostActiveSettings.Data) state =
    let guildId = guild.Id
    match Map.tryFind guildId state.Rankings with
    | None -> state
    | Some rankings ->
        let mostActive =
            rankings
            |> Seq.fold
                (fun lastMostActive (KeyValue(userId, user)) ->
                    match lastMostActive with
                    | Some (lastMostActive: Rankings.RankingData) ->
                        if user.DayExp > lastMostActive.DayExp then
                            user
                        else
                            lastMostActive
                    | None -> user
                    |> Some
                )
                None

        match mostActive with
        | Some mostActiveUser ->
            let update mostActiveSetting =
                MostActiveSettings.replace mostActiveSetting

                let rankings =
                    rankings
                    |> Map.map (fun _ user ->
                        let user =
                            { user with DayExp = 0UL }

                        Rankings.replace user

                        user
                    )

                { state with
                    Rankings = Map.add guildId rankings state.Rankings
                    MostActiveSettings = Map.add guildId mostActiveSetting state.MostActiveSettings
                }

            if mostActiveUser.UserId = mostActiveSetting.LastMostActiveUserId then
                { mostActiveSetting with
                    LastUpdate = System.DateTime.UtcNow
                }
                |> update
            else
                match guild.GetRole mostActiveSetting.MostActiveRoleId with
                | null -> ()
                | mostActiveRole ->
                    let getMember userId =
                        // DSharpPlus.Exceptions.NotFoundException: Not found: 404
                        try await <| guild.GetMemberAsync userId |> Some with e -> None

                    match getMember mostActiveSetting.LastMostActiveUserId with
                    | Some guildMember ->
                        try
                            guildMember.RevokeRoleAsync(mostActiveRole).GetAwaiter().GetResult()
                        with _ -> ()
                    | None -> ()

                    match getMember mostActiveUser.UserId with
                    | Some guildMember ->
                        try
                            guildMember.GrantRoleAsync(mostActiveRole).GetAwaiter().GetResult()
                        with _ -> ()
                    | None -> ()

                { mostActiveSetting with
                    LastUpdate = System.DateTime.UtcNow
                    LastMostActiveUserId = mostActiveUser.UserId
                }
                |> update

        | None ->
            state

[<Literal>]
let MostActiveLeaderboardRefreshButtonId = "MostActiveLeaderboardRefreshButtonId"
[<Literal>]
let MostActiveLeftArrowButtonId = "MostActiveLeftArrowButtonId"
[<Literal>]
let MostActivePaginationButtonId = "MostActivePaginationButtonId"
[<Literal>]
let MostActiveRightArrowButtonId = "MostActiveRightArrowButtonId"

[<Struct>]
type SortBy =
    | SortByExp
    | SortByDayExp
    | SortByIndex

let createMostActiveLeaderboard
    guildId
    setContent
    (addComponents: Entities.DiscordComponent [] -> _)
    addEmbed
    page
    state =

    match Map.tryFind guildId state.MostActiveSettings with
    | Some mostActiveSetting ->
        match Map.tryFind guildId state.Rankings with
        | Some ranking ->
            if Map.isEmpty ranking then
                setContent "Not found any rank"
            else
                let itemsCountPerPage = 10
                let sortBy = SortByDayExp

                let itemsCount = ranking.Count
                let lastPageItemsCount = itemsCount % itemsCountPerPage
                let pagesCount = itemsCount / itemsCountPerPage + if lastPageItemsCount > 0 then 1 else 0

                let table =
                    let sortFunction: Map<UserId, Rankings.RankingData> -> _ =
                        match sortBy with
                        | SortByDayExp -> Seq.sortByDescending (fun x -> x.Value.DayExp)
                        | SortByExp -> Seq.sortByDescending (fun x -> x.Value.Exp)
                        | SortByIndex -> Seq.cast

                    let headers = [| "Учасники"; "ОбщОпыт"; "СутОпыт" |]

                    let table =
                        if 0 < page && page <= pagesCount then
                            let lb = (page - 1) * itemsCountPerPage
                            let ub =
                                if page = pagesCount then if lastPageItemsCount > 0 then lastPageItemsCount else itemsCountPerPage
                                else itemsCountPerPage

                            ranking
                            |> sortFunction
                            |> Seq.skip lb |> Seq.take ub
                            |> Seq.mapi (fun i (KeyValue(userId, user)) ->
                                [|
                                    sprintf "%d <@!%d>" (lb + i + 1) userId
                                    string user.Exp
                                    string user.DayExp
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
                            .WithDescription(
                                sprintf "Суточная статистика опыта с <t:%d:D>:"
                                    (DateTime.Unix.toSec mostActiveSetting.LastUpdate)
                            )
                    let b =
                        table
                        |> Array.fold
                            (fun (b: Entities.DiscordEmbedBuilder) (header, value) ->
                                b.AddField(header, value, true)
                            )
                            b

                    b.Build()

                addComponents [|
                    Entities.DiscordButtonComponent(
                        ButtonStyle.Secondary,
                        MostActiveLeaderboardRefreshButtonId,
                        "",
                        emoji = Entities.DiscordComponentEmoji(Name = "🔄") // :arrows_counterclockwise:
                    )

                    Entities.DiscordButtonComponent(
                        ButtonStyle.Secondary,
                        MostActiveLeftArrowButtonId,
                        "",
                        disabled = (page <= 1),
                        emoji = Entities.DiscordComponentEmoji(Name = "⬅️")
                    )
                    Entities.DiscordButtonComponent(
                        ButtonStyle.Secondary,
                        MostActivePaginationButtonId,
                        sprintf "%d/%d" page pagesCount,
                        disabled = true
                    )
                    Entities.DiscordButtonComponent(
                        ButtonStyle.Secondary,
                        MostActiveRightArrowButtonId,
                        "",
                        disabled = (page >= pagesCount),
                        emoji = Entities.DiscordComponentEmoji(Name = "➡️")
                    )
                |]
                |> ignore

                addEmbed embed |> ignore
        | None ->
            setContent "Not found any rank"
    | None ->
        setContent "Most active settings not set!"

let requestReduce
    (e: EventArgs.MessageCreateEventArgs)
    (msg: Request)
    (state: State): State =

    match msg with
    | GetRank userId ->
        let msg =
            let userId = match userId with None -> e.Author.Id | Some userId -> userId

            let calc exp =
                let level = getLevelBySumExp exp

                sprintf "Уровень <@!%d> — %d.\nОпыта %d / %d до следующего уровня.\nВсего опыта — %d."
                    userId
                    level
                    (exp - getLevelTotalExp level)
                    (getExpByLevel level)
                    exp

            match Map.tryFind e.Guild.Id state.Rankings with
            | Some ranking ->
                match Map.tryFind userId ranking with
                | Some r -> calc r.Exp
                | None -> calc 0UL
            | None -> calc 0UL

        let b = Entities.DiscordMessageBuilder()

        let color = Entities.DiscordColor "#2f3136"

        let embed =
            Entities.DiscordEmbedBuilder()
                .WithDescription(msg)
                .WithColor(color)
                .Build()

        b.WithEmbed embed |> ignore

        awaiti (e.Channel.SendMessageAsync(b))

        state

    | SetExp(targetUserId, exp) ->
        let guild = e.Guild
        let currentMember = await (guild.GetMemberAsync(e.Author.Id))
        let replyMessage =
            await (e.Channel.SendMessageAsync("Processing..."))

        if currentMember.Permissions &&& Permissions.Administrator = Permissions.Administrator then
            match Map.tryFind guild.Id state.Settings with
            | Some setting ->
                let state =
                    state
                    |> updateExp
                        e
                        targetUserId
                        0L
                        setting
                        (fun _ -> exp)

                let msg = sprintf "User <@!%d> exp has been set to %d" targetUserId exp
                awaiti (replyMessage.ModifyAsync(Entities.Optional msg))

                state
            | None ->
                awaiti (replyMessage.ModifyAsync(Entities.Optional "Firstly set up the ranking system"))

                state
        else
            awaiti (replyMessage.ModifyAsync(Entities.Optional("You don't have permission")))

            state

    | SettingRequest settingMsg ->
        rankingSettingReduce e settingMsg state

    | MostActiveSettingRequest settingMsg ->
        mostActiveSettingReduce e settingMsg state

    | MostActiveActivate ->
        let guild = e.Guild
        let currentMember = await (guild.GetMemberAsync(e.Author.Id))
        let replyMessage =
            await (e.Channel.SendMessageAsync "Processing...")

        if currentMember.Permissions &&& Permissions.Administrator = Permissions.Administrator then
            match Map.tryFind e.Guild.Id state.MostActiveSettings with
            | Some mostActiveSetting ->
                let state =
                    mostActiveActivate guild mostActiveSetting state

                awaiti <| replyMessage.ModifyAsync(Entities.Optional "Done!")

                state
            | None ->
                awaiti <| replyMessage.ModifyAsync(Entities.Optional "Most active settings not set!")

                state
        else
            awaiti (replyMessage.ModifyAsync(Entities.Optional("You don't have permission")))

            state

    | MostActiveLeaderboard ->
        e.Channel.TriggerTypingAsync().GetAwaiter().GetResult()

        let b = Entities.DiscordMessageBuilder()

        createMostActiveLeaderboard e.Guild.Id (fun content -> b.Content <- content) b.AddComponents b.AddEmbed 1 state

        awaiti <| e.Channel.SendMessageAsync b

        state

let reduce (msg: Msg) (state: State): State =
    match msg with
    | NewMessageHandle e ->
        if e.Author.IsBot then state
        else
            match Map.tryFind e.Guild.Id state.Settings with
            | Some setting ->
                state
                |> updateExp
                    e
                    e.Author.Id
                    coolDown
                    setting
                    (fun currExp ->
                        currExp + uint64 (r.Next(lowerBoundExp, upperBoundExp + 1)))

            | None -> state

    | Request (e, msg) ->
        requestReduce e msg state

    | MostActiveActivateAll client ->
        state.MostActiveSettings
        |> Map.fold
            (fun state guildId mostActiveSetting ->
                let guild =
                    try
                        await <| client.GetGuildAsync guildId
                        |> Some
                    with e -> None

                match guild with
                | Some guild ->
                    mostActiveActivate guild mostActiveSetting state
                | None -> state
            )
            state
    | GetState r ->
        r.Reply state

        state
let m =
    let init = {
        Settings = RankingSettings.getAll ()
        Rankings = Rankings.getAll ()
        CoolDowns = Map.empty
        MostActiveSettings = MostActiveSettings.getAll ()
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

let handle (e: EventArgs.MessageCreateEventArgs) =
    m.Post (NewMessageHandle e)

let execSettingCmd e msg =
    m.Post (Request (e, msg))

let componentInteractionCreateHandle (client: DiscordClient) (e: EventArgs.ComponentInteractionCreateEventArgs) =
    let getCurrentPage () =
        e.Message.Components
        |> Seq.tryPick (fun row ->
            row.Components
            |> Seq.tryPick (fun row ->
                if row.CustomId = MostActivePaginationButtonId then
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

        let state = m.PostAndReply GetState

        let b = Entities.DiscordInteractionResponseBuilder()

        createMostActiveLeaderboard e.Guild.Id (fun content -> b.Content <- content) b.AddComponents b.AddEmbed (getPage currentPage) state

        e.Interaction.CreateResponseAsync(InteractionResponseType.UpdateMessage, b).GetAwaiter().GetResult()


    if e.Message.Author.Id = client.CurrentUser.Id then
        match e.Id with
        | MostActiveLeaderboardRefreshButtonId ->
            update id
            true
        | MostActiveLeftArrowButtonId ->
            update (fun currentPage -> currentPage - 1)
            true
        | MostActiveRightArrowButtonId ->
            update (fun currentPage -> currentPage + 1)
            true
        | _ -> false

    else
        false

module Parser =
    open FParsec

    open DiscordMessage.Parser

    type 'Result Parser = Primitives.Parser<'Result, unit>

    let psetOutputChannel: _ Parser =
        skipStringCI "setRankingOutput" >>. spaces
        >>. (pchannelMention <|> puint64)

    let psetExp: _ Parser =
        skipStringCI "setExp" >>. spaces
        >>. tuple2
                ((puserMention <|> puint64) .>> spaces)
                puint64

    let psetLevelRoles: _ Parser =
        let proleIdLevel =
            tuple2
                ((pmentionRole <|> puint64) .>> spaces)
                pint32

        skipStringCI "setLevelRoles" >>. spaces
        >>. many (proleIdLevel .>> spaces)

    let pgetLevelRoles: _ Parser =
        skipStringCI "getLevelRoles"

    let pgetRank: _ Parser =
        skipStringCI "rank" >>. spaces
        >>. opt (puserMention <|> puint64)

    let pmostActiveSetRole: _ Parser =
        skipStringCI "mostActiveSetRole" >>. spaces
        >>. (pmentionRole <|> puint64)

    let pmostActiveGetRole: _ Parser =
        skipStringCI "mostActiveGetRole"

    let pmostActiveActivate: _ Parser =
        skipStringCI "mostActiveActivate"

    let pmostActiveLeaderboard: _ Parser =
        skipStringCI "mostActiveLeaderboard"

    let start: _ Parser =
        let prankingSetting =
            choice [
                psetOutputChannel |>> SetOutputChannel
                psetLevelRoles |>> SetLevelRoles
                pgetLevelRoles >>% GetLevelRoles
            ]

        let pmostActiveSetting =
            choice [
                pmostActiveSetRole |>> SetMostActiveRole
                pmostActiveGetRole >>% GetMostActiveRole
            ]

        choice [
            pgetRank |>> GetRank
            psetExp |>> SetExp
            prankingSetting |>> SettingRequest

            pmostActiveSetting |>> MostActiveSettingRequest
            pmostActiveActivate >>% MostActiveActivate
            pmostActiveLeaderboard >>% MostActiveLeaderboard
        ]

let mostActiveTimerStart (client: DiscordClient) =
    let scheduler = new Scheduler.Scheduler<unit>(Scheduler.State.Empty)

    let nextDay (now: System.DateTime) =
        System.DateTime(now.Year, now.Month, now.Day + 1, 0, 0, 0)

    scheduler.AddJob { Time = nextDay System.DateTime.Now; Type = () }

    Scheduler.startAsync scheduler 100 (fun job ->
        m.Post (MostActiveActivateAll client)

        scheduler.AddJob { Time = nextDay job.Time; Type = () }
    )
    |> ignore
