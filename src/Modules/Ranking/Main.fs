module Ranking.Main
open FsharpMyExtension
open DSharpPlus

open Types
open Extensions
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
        Settings: RankingSettings.Guilds
        Rankings: Rankings.GuildUsers
        CoolDowns: Map<UserId, Ticks>
        MostActiveSettings: MostActiveSettings.Guilds
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
    (setting: RankingSettings.GuildData)
    update
    (state: State) =

    let id = Rankings.Id.create e.Guild.Id userId

    let exec (rank: Rankings.GuildUserData) (state: State) =
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
                        | None -> DiscordEmbed.backgroundColorDarkTheme
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

        { state with
            Rankings =
                state.Rankings
                |> Rankings.GuildUsers.set
                    id
                    (fun _ -> newRank)
        }

    match Rankings.GuildUsers.tryFindById id state.Rankings with
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

                exec rank.Data state
        | None ->
            let now = System.DateTime.Now.Ticks
            let state =
                { state with
                    CoolDowns = Map.add userId now state.CoolDowns
                }

            exec rank.Data state
    | None ->
        let now = System.DateTime.Now.Ticks
        let state =
            { state with
                CoolDowns = Map.add userId now state.CoolDowns
            }

        exec Rankings.GuildUserData.Empty state

let rankingSettingReduce
    (e: EventArgs.MessageCreateEventArgs)
    (msg: RankingSettingRequest)
    (state: State): State =

    match msg with
    | SetOutputChannel outputChannelId ->
        let guild = e.Guild
        let currentMember = getGuildMember guild e.Author
        let replyMessage =
            await (e.Channel.SendMessageAsync("Processing..."))

        if currentMember.Permissions &&& Permissions.Administrator = Permissions.Administrator then
            let settings =
                state.Settings
                |> RankingSettings.Guilds.set
                    e.Guild.Id
                    (fun settings ->
                        { settings with
                            OutputChannelId = Some outputChannelId
                        }
                    )

            awaiti (replyMessage.ModifyAsync(Entities.Optional("Output channel for ranking has been set")))

            { state with Settings = settings }
        else
            awaiti (replyMessage.ModifyAsync(Entities.Optional("You don't have permission")))

            state

    | SetLevelRoles levelRoles ->
        let guild = e.Guild
        let currentMember = getGuildMember guild e.Author
        let replyMessage =
            await (e.Channel.SendMessageAsync("Processing..."))

        if currentMember.Permissions &&& Permissions.Administrator = Permissions.Administrator then
            let levelRoles =
                List.sort levelRoles
                |> List.map (fun (roleId, level) ->
                    { Role = roleId; Level = level } : RankingSettings.LevelRole
                )
                |> Array.ofList

            let settings =
                state.Settings
                |> RankingSettings.Guilds.set
                    e.Guild.Id
                    (fun settings ->
                        { settings with
                            LevelRoles = levelRoles
                        }
                    )

            awaiti (replyMessage.ModifyAsync(Entities.Optional("Roles level has been set")))

            { state with Settings = settings }

        else
            awaiti (replyMessage.ModifyAsync(Entities.Optional("You don't have permission")))

            state

    | GetLevelRoles ->
        let guild = e.Guild
        let replyMessage =
            await (e.Channel.SendMessageAsync("Processing..."))

        match RankingSettings.Guilds.tryFindById guild.Id state.Settings with
        | Some setting ->
            let msg =
                setting.Data.LevelRoles
                |> Array.map (fun x -> sprintf "<@&%d> — %d" x.Role x.Level)
                |> String.concat "\n"

            let b = Entities.DiscordMessageBuilder()

            let color = DiscordEmbed.backgroundColorDarkTheme

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
        let currentMember = getGuildMember guild e.Author
        let replyMessage =
            await (e.Channel.SendMessageAsync "Processing...")

        if currentMember.Permissions &&& Permissions.Administrator = Permissions.Administrator then
            let settings =
                state.MostActiveSettings
                |> MostActiveSettings.Guilds.set
                    e.Guild.Id
                    (fun setting ->
                        { setting with
                            MostActiveRoleId = roleId
                        }
                    )

            awaiti (replyMessage.ModifyAsync(Entities.Optional("Most active role has been set")))

            { state with MostActiveSettings = settings }

        else
            awaiti (replyMessage.ModifyAsync(Entities.Optional("You don't have administrative permission")))

            state

    | GetMostActiveRole ->
        let guild = e.Guild
        let replyMessage =
            await (e.Channel.SendMessageAsync "Processing...")

        match MostActiveSettings.Guilds.tryFindById guild.Id state.MostActiveSettings with
        | Some setting ->
            let b = Entities.DiscordMessageBuilder()

            let embed =
                Entities.DiscordEmbedBuilder()
                    .WithDescription(sprintf "<@&%d>" setting.Data.MostActiveRoleId)
                    .WithColor(DiscordEmbed.backgroundColorDarkTheme)
                    .Build()

            b.WithEmbed embed |> ignore

            awaiti (replyMessage.ModifyAsync(b))
        | None ->
            awaiti (replyMessage.ModifyAsync(Entities.Optional "A most active role is not set"))

        state

let mostActiveActivate (guild: Entities.DiscordGuild) (mostActiveSetting: MostActiveSettings.GuildData) state =
    let guildId = guild.Id
    match Rankings.GuildUsers.tryFindGuildUsers guildId state.Rankings with
    | None -> state
    | Some guildUsers ->
        let mostActive =
            guildUsers
            |> Array.fold
                (fun lastMostActive currentUser ->
                    match lastMostActive with
                    | Some (lastMostActive: Rankings.GuildUser) ->
                        if currentUser.Data.DayExp > lastMostActive.Data.DayExp then
                            currentUser
                        else
                            lastMostActive
                    | None -> currentUser
                    |> Some
                )
                None

        match mostActive with
        | Some mostActiveUser ->
            let update mostActiveSetting =
                let guildUsersSetDayExpToZero =
                    guildUsers
                    |> Array.choose (fun guildUser ->
                        if guildUser.Data.DayExp = 0UL then
                            None
                        else
                            guildUser
                            |> Rankings.GuildUser.update
                                (fun data ->
                                    { data with
                                        DayExp = 0UL
                                    }
                                )
                            |> Some
                    )
                    |> Rankings.GuildUsers.sets

                { state with
                    Rankings =
                        guildUsersSetDayExpToZero state.Rankings
                    MostActiveSettings =
                        state.MostActiveSettings
                        |> MostActiveSettings.Guilds.set
                            guildId
                            (fun _ -> mostActiveSetting)
                }

            if mostActiveUser.Id.UserId = mostActiveSetting.LastMostActiveUserId then
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
                            awaiti <| guildMember.RevokeRoleAsync(mostActiveRole)
                        with _ -> ()
                    | None -> ()

                    match getMember mostActiveUser.Id.UserId with
                    | Some guildMember ->
                        try
                            awaiti <| guildMember.GrantRoleAsync(mostActiveRole)
                        with _ -> ()
                    | None -> ()

                { mostActiveSetting with
                    LastUpdate = System.DateTime.UtcNow
                    LastMostActiveUserId = mostActiveUser.Id.UserId
                }
                |> update

        | None ->
            state

module MostActiveTable =
    open Shared.Ui.Table

    type SortBy =
        | SortByDayExp = 0
        | SortByExp = 1
        | SortByIndex = 2

    let initSetting getState: Setting<_, SortBy, _, (GuildId * State)> =
        {
            Id = "MostActive"

            GetState = getState

            Title = fun _ _ -> "Наиболее активные ребята"

            GetHeaders = fun sortBy ->
                match sortBy with
                | SortBy.SortByDayExp ->
                    [| "Учасники"; "ОбщОпыт"; "СутОпыт▼" |]
                | SortBy.SortByExp ->
                    [| "Учасники"; "ОбщОпыт▼"; "СутОпыт" |]
                | SortBy.SortByIndex ->
                    [| "Учасники▼"; "ОбщОпыт"; "СутОпыт" |]
                | x ->
                    failwithf "MostActiveTable.SortBy %A" x

            GetItems = fun () (guildId, state) ->
                match Rankings.GuildUsers.tryFindGuildUsers guildId state.Rankings with
                | Some ranking -> ranking
                | None -> [||]

            ItemsCountPerPage = 10

            SortBy = SortByContainer.Init [|
                SortBy.SortByDayExp, "Отсортировать по сут. опыту"
                SortBy.SortByExp, "Отсортировать по общ. опыту"
                SortBy.SortByIndex, "Отсортировать по общ. индексу"
            |]

            SortFunction = fun sortBy items ->
                match sortBy with
                | SortBy.SortByDayExp ->
                    Array.sortByDescending (fun user -> user.Data.DayExp) items
                | SortBy.SortByExp ->
                    Array.sortByDescending (fun user -> user.Data.Exp) items
                | SortBy.SortByIndex -> items
                | x ->
                    failwithf "MostActiveTable.SortBy %A" x

            MapFunction =
                fun _ i user ->
                    [|
                        sprintf "%d <@!%d>" i user.Id.UserId
                        string user.Data.Exp
                        string user.Data.DayExp
                    |]
        }

    let createTable
        (addComponents: Entities.DiscordComponent [] -> _)
        addEmbed
        userRanks =

        createTable addComponents addEmbed 1 (None, ()) (initSetting userRanks)

    let componentInteractionCreateHandle (client: DiscordClient) (e: EventArgs.ComponentInteractionCreateEventArgs) getState =
        let getState () =
            let state = getState ()
            e.Guild.Id, state

        componentInteractionCreateHandle client e (initSetting getState)

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

            let id = Rankings.Id.create e.Guild.Id userId
            match Rankings.GuildUsers.tryFindById id state.Rankings with
            | Some ranking ->
                calc ranking.Data.Exp
            | None -> calc 0UL

        let b = Entities.DiscordMessageBuilder()

        let color = DiscordEmbed.backgroundColorDarkTheme

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
        let currentMember = getGuildMember guild e.Author
        let replyMessage =
            await (e.Channel.SendMessageAsync("Processing..."))

        if currentMember.Permissions &&& Permissions.Administrator = Permissions.Administrator then
            match RankingSettings.Guilds.tryFindById guild.Id state.Settings with
            | Some setting ->
                let state =
                    state
                    |> updateExp
                        e
                        targetUserId
                        0L
                        setting.Data
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
        let currentMember = getGuildMember guild e.Author
        let replyMessage =
            await (e.Channel.SendMessageAsync "Processing...")

        if currentMember.Permissions &&& Permissions.Administrator = Permissions.Administrator then
            match MostActiveSettings.Guilds.tryFindById e.Guild.Id state.MostActiveSettings with
            | Some mostActiveSetting ->
                let state =
                    mostActiveActivate guild mostActiveSetting.Data state

                awaiti <| replyMessage.ModifyAsync(Entities.Optional "Done!")

                state
            | None ->
                awaiti <| replyMessage.ModifyAsync(Entities.Optional "Most active settings not set!")

                state
        else
            awaiti (replyMessage.ModifyAsync(Entities.Optional("You don't have permission")))

            state

    | MostActiveLeaderboard ->
        awaiti <| e.Channel.TriggerTypingAsync()

        let b = Entities.DiscordMessageBuilder()

        let getState () =
            e.Guild.Id, state

        MostActiveTable.createTable b.AddComponents b.AddEmbed getState

        awaiti <| e.Channel.SendMessageAsync b

        state

let reduce (msg: Msg) (state: State): State =
    match msg with
    | NewMessageHandle e ->
        if e.Author.IsBot then state
        else
            match RankingSettings.Guilds.tryFindById e.Guild.Id state.Settings with
            | Some setting ->
                state
                |> updateExp
                    e
                    e.Author.Id
                    coolDown
                    setting.Data
                    (fun currExp ->
                        currExp + uint64 (r.Next(lowerBoundExp, upperBoundExp + 1)))

            | None -> state

    | Request (e, msg) ->
        requestReduce e msg state

    | MostActiveActivateAll client ->
        state.MostActiveSettings.Cache
        |> Map.fold
            (fun state guildId mostActiveSetting ->
                let guild =
                    try
                        await <| client.GetGuildAsync guildId
                        |> Some
                    with e -> None

                match guild with
                | Some guild ->
                    mostActiveActivate guild mostActiveSetting.Data state
                | None -> state
            )
            state
    | GetState r ->
        r.Reply state

        state
let m =
    let init = {
        Settings = RankingSettings.Guilds.init "guildRankingSettings" Db.database
        Rankings = Rankings.GuildUsers.init "guildRankings" Db.database
        CoolDowns = Map.empty
        MostActiveSettings = MostActiveSettings.Guilds.init "mostActiveSettings" Db.database
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

let componentInteractionCreateHandle client e =
    MostActiveTable.componentInteractionCreateHandle
        client
        e
        (fun () -> m.PostAndReply GetState)

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

    let start f: _ Parser =
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
        >>= fun msg ->
            preturn (fun x -> f x msg)

let exec: MessageCreateEventHandler Parser.Parser =
    Parser.start (fun (client: DiscordClient, e: EventArgs.MessageCreateEventArgs) msg ->
        m.Post (Request (e, msg))
    )

let mostActiveTimerStart (client: DiscordClient) =
    let scheduler = new Scheduler.Scheduler<unit>(Scheduler.State.Empty)

    let nextDay (now: System.DateTime) =
        let now = now.AddDays 1.0
        System.DateTime(now.Year, now.Month, now.Day, 0, 0, 0)

    scheduler.AddJob { Time = nextDay System.DateTime.Now; Type = () }

    Scheduler.startAsync scheduler 100 (fun job ->
        m.Post (MostActiveActivateAll client)

        scheduler.AddJob { Time = nextDay job.Time; Type = () }
    )
    |> ignore
