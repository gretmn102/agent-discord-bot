module Ranking.Main
open DSharpPlus

open Types
open Model

type Msg =
    | NewMessageHandle of EventArgs.MessageCreateEventArgs

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

let reduce (msg: Msg) (state: State): State =
    match msg with
    | NewMessageHandle e ->
        if e.Author.IsBot then state
        else
            match Map.tryFind e.Guild.Id state.Settings with
            | Some setting ->
                match Map.tryFind e.Guild.Id state.Rankings with
                | Some rankings ->
                    let exec (rank: Rankings.RankingData) =
                        let oldLevel = getLevelByExp rank.Exp
                        let newExp = rank.Exp + uint64 (r.Next(lowerBoundExp, upperBoundExp + 1))
                        let newLevel = getLevelByExp newExp

                        if oldLevel <> newLevel then
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
                | None -> state
            | None -> state

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
