module Ranking.Main
open DSharpPlus

open Types
open Model

type SettingRequest =
    | SetOutputChannel of ChannelId
    | SetLevelRoles of (ChannelId * int) list
    | GetLevelRoles

type Request =
    | SetExp of UserId * uint64
    | GetRank of UserId option
    | SettingRequest of SettingRequest

type Msg =
    | NewMessageHandle of EventArgs.MessageCreateEventArgs
    | Request of EventArgs.MessageCreateEventArgs * Request

type Ticks = int64

type State =
    {
        Settings: RankingSettings.GuildRankingSettings
        Rankings: Rankings.GuildRankings
        CoolDowns: Map<UserId, Ticks>
    }

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

let settingReduce
    (e: EventArgs.MessageCreateEventArgs)
    (msg: SettingRequest)
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
        settingReduce e settingMsg state

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

let m =
    let init = {
        Settings = RankingSettings.getAll ()
        Rankings = Rankings.getAll ()
        CoolDowns = Map.empty
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

    let start: _ Parser =
        let psetting =
            choice [
                psetOutputChannel |>> SetOutputChannel
                psetLevelRoles |>> SetLevelRoles
                pgetLevelRoles >>% GetLevelRoles
            ]
        choice [
            pgetRank |>> GetRank
            psetExp |>> SetExp
            psetting |>> SettingRequest
        ]
