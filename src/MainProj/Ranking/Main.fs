module Ranking.Main
open DSharpPlus

open Types
open Model

type SettingMsg =
    | SetOutputChannel of ChannelId

type Msg =
    | NewMessageHandle of EventArgs.MessageCreateEventArgs
    | SettingMsg of EventArgs.MessageCreateEventArgs * SettingMsg

type State =
    {
        Settings: RankingSettings.GuildRankingSettings
        Rankings: Rankings.GuildRankings
    }

let lowerBoundExp, upperBoundExp = 15, 25
let getExpByLevel level = 5 * (level * level) + (50 * level) + 100
let inline getLevelByExp exp =
    int (sqrt (125. + 5. * float exp) / 5. - 5.)

let r = System.Random ()

let settingReduce
    (e: EventArgs.MessageCreateEventArgs)
    (msg: SettingMsg)
    (state: RankingSettings.GuildRankingSettings): RankingSettings.GuildRankingSettings =

    match msg with
    | SetOutputChannel outputChannelId ->
        let guild = e.Guild
        let currentMember = await (guild.GetMemberAsync(e.Author.Id))
        let replyMessage =
            await (e.Channel.SendMessageAsync("Processing..."))

        if currentMember.Permissions &&& Permissions.Administrator = Permissions.Administrator then
            let state: RankingSettings.GuildRankingSettings =
                match Map.tryFind e.Guild.Id state with
                | Some setting ->
                    let newcomersRoles =
                        { setting with
                            OutputChannelId = Some outputChannelId
                        }

                    RankingSettings.replace newcomersRoles

                    Map.add guild.Id newcomersRoles state
                | None ->
                    let x = RankingSettings.insert (guild.Id, [||], Some outputChannelId)
                    Map.add guild.Id x state

            awaiti (replyMessage.ModifyAsync(Entities.Optional("Output channel for ranking has been set")))

            state
        else
            awaiti (replyMessage.ModifyAsync(Entities.Optional("You don't have permission")))

            state

let reduce (msg: Msg) (state: State): State =
    match msg with
    | NewMessageHandle e ->
        if e.Author.IsBot then state
        else
            match Map.tryFind e.Guild.Id state.Settings with
            | Some setting ->
                let exec rankings =
                    let exec (rank: Rankings.RankingData) =
                        let oldLevel = getLevelByExp rank.Exp
                        let newExp = rank.Exp + uint64 (r.Next(lowerBoundExp, upperBoundExp + 1))
                        let newLevel = getLevelByExp newExp

                        if newLevel > 0 && oldLevel <> newLevel then
                            match setting.OutputChannelId with
                            | Some outputChannelId ->
                                match e.Guild.GetChannel outputChannelId with
                                | null -> ()
                                | outputChannel ->
                                    let msg = sprintf "%s поднялся до %d уровня." e.Author.Username newLevel
                                    awaiti (outputChannel.SendMessageAsync(msg))
                            | None -> ()

                            // TODO: дать роль, если она подошла по условиям

                        let newRank =
                            { rank with Exp = newExp }

                        Rankings.replace newRank

                        { state with
                            Rankings =
                                let m = Map.add e.Author.Id newRank rankings
                                Map.add e.Guild.Id m state.Rankings
                        }

                    match Map.tryFind e.Author.Id rankings with
                    | Some rank ->
                        exec rank
                    | None ->
                        let rank = Rankings.insert(e.Guild.Id, e.Author.Id, 0UL)

                        exec rank

                match Map.tryFind e.Guild.Id state.Rankings with
                | Some rankings ->
                    exec rankings
                | None ->
                    exec Map.empty
            | None -> state

    | SettingMsg (e, msg) ->
        { state with
            Settings =
                settingReduce e msg state.Settings
        }

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
        >>. (pmentionRole <|> puint64)

    let start: _ Parser =
        choice [
            psetOutputChannel |>> SetOutputChannel
        ]
