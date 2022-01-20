module Ranking.Main
open DSharpPlus

open Types
open Model

type SettingMsg =
    | SetOutputChannel of ChannelId
    | SetExp of UserId * uint64
    | SetLevelRoles of (ChannelId * int) list
    | GetLevelRoles

type Msg =
    | NewMessageHandle of EventArgs.MessageCreateEventArgs
    | SettingMsg of EventArgs.MessageCreateEventArgs * SettingMsg

type State =
    {
        Settings: RankingSettings.GuildRankingSettings
        Rankings: Rankings.GuildRankings
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

let r = System.Random ()

let updateExp
    (e: EventArgs.MessageCreateEventArgs)
    (userId: UserId)
    (setting: RankingSettings.RankingSettingData)
    update
    state =

    let exec rankings =
        let exec (rank: Rankings.RankingData) =
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

                        awaiti (outputChannel.SendMessageAsync(b))

                | None -> ()

            let newRank =
                { rank with Exp = newExp }

            Rankings.replace newRank

            { state with
                Rankings =
                    let m = Map.add userId newRank rankings
                    Map.add e.Guild.Id m state.Rankings
            }

        match Map.tryFind userId rankings with
        | Some rank ->
            exec rank
        | None ->
            let rank = Rankings.insert(e.Guild.Id, userId, 0UL)

            exec rank

    match Map.tryFind e.Guild.Id state.Rankings with
    | Some rankings ->
        exec rankings
    | None ->
        exec Map.empty

let settingReduce
    (e: EventArgs.MessageCreateEventArgs)
    (msg: SettingMsg)
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
                    setting
                    (fun currExp ->
                        currExp + uint64 (r.Next(lowerBoundExp, upperBoundExp + 1)))

            | None -> state

    | SettingMsg (e, msg) ->
        settingReduce e msg state

let m =
    let init = {
        Settings = RankingSettings.getAll ()
        Rankings = Rankings.getAll ()
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
    m.Post (SettingMsg (e, msg))

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
                ((pchannelMention <|> puint64) .>> spaces)
                pint32

        skipStringCI "setLevelRoles" >>. spaces
        >>. many (proleIdLevel .>> spaces)

    let pgetLevelRoles: _ Parser =
        skipStringCI "getLevelRoles"

    let start: _ Parser =
        choice [
            psetOutputChannel |>> SetOutputChannel
            psetExp |>> SetExp
            psetLevelRoles |>> SetLevelRoles
            pgetLevelRoles >>% GetLevelRoles
        ]
