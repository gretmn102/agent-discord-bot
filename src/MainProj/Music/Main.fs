module Music.Main
open DSharpPlus
open DSharpPlus.VoiceNext

open Types

type Request =
    | Join
    | Leave

type Msg = DiscordClient * EventArgs.MessageCreateEventArgs * Request

type State = { Empty: unit }

let reduce (msg: Msg) (state: State) =
    match msg with
    | (client: DiscordClient), (e: EventArgs.MessageCreateEventArgs), msg ->
        match client.GetVoiceNext() with
        | null ->
            awaiti <| e.Channel.SendMessageAsync("VNext is not enabled or configured.")

        | vnext ->
            match msg with
            | Join ->
                match vnext.GetConnection(e.Guild) with
                | null ->
                    let guildMember = await <| e.Guild.GetMemberAsync(e.Author.Id)
                    match guildMember.VoiceState with
                    | null ->
                        awaiti <| e.Channel.SendMessageAsync("You are not in a voice channel.")
                    | vstat ->
                        match vstat.Channel with
                        | null ->
                            awaiti <| e.Channel.SendMessageAsync("You are not in a voice channel.")
                        | chn ->
                            awaiti <| vnext.ConnectAsync(chn)
                | vnc ->
                    awaiti <| e.Channel.SendMessageAsync("Already connected in this guild.")

            | Leave ->
                match vnext.GetConnection(e.Guild) with
                | null ->
                    awaiti <| e.Channel.SendMessageAsync("Not connected in this guild.")
                | vnc ->
                    vnc.Disconnect()

        state

let m =
    let init = {
        Empty = ()
    }

    MailboxProcessor.Start (fun mail ->
        let rec loop (state: State) =
            async {
                let! (msg: Msg) = mail.Receive()
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

let exec (client: DiscordClient) (e: EventArgs.MessageCreateEventArgs) msg =
    m.Post (client, e, msg)

module Parser =
    open FParsec

    open DiscordMessage.Parser

    type 'Result Parser = Primitives.Parser<'Result, unit>

    let pjoin: _ Parser =
        skipStringCI "join"

    let pleave: _ Parser =
        skipStringCI "leave"

    let start =
        choice [
            pjoin >>% Join
            pleave >>% Leave
        ]
