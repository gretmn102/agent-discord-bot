module Music.Main
open DSharpPlus
open DSharpPlus.VoiceNext

open Types

type Request =
    | Join
    | Leave
    | Play of string
    | Stop

type Msg = DiscordClient * EventArgs.MessageCreateEventArgs * Request

type State = {
    CancelationToken: System.Threading.CancellationTokenSource
}

let reduce (msg: Msg) (state: State) =
    match msg with
    | (client: DiscordClient), (e: EventArgs.MessageCreateEventArgs), msg ->
        match client.GetVoiceNext() with
        | null ->
            awaiti <| e.Channel.SendMessageAsync("VNext is not enabled or configured.")
            state
        | vnext ->
            match msg with
            | Join ->
                match vnext.GetConnection(e.Guild) with
                | null ->
                    let guildMember = getGuildMember e.Guild e.Author
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

                state
            | Leave ->
                match vnext.GetConnection(e.Guild) with
                | null ->
                    awaiti <| e.Channel.SendMessageAsync("Not connected in this guild.")
                | vnc ->
                    vnc.Disconnect()

                state
            | Play path ->
                match vnext.GetConnection(e.Guild) with
                | null ->
                    awaiti <| e.Channel.SendMessageAsync("Not connected in this guild.")

                    state
                | vnc ->
                    let cancelToken =
                        if vnc.IsPlaying then
                            state.CancelationToken.Cancel()

                            new System.Threading.CancellationTokenSource()
                        else
                            state.CancelationToken

                    awaiti <| vnc.WaitForPlaybackFinishAsync()

                    async {
                        try
                            do! Async.AwaitTask(vnc.SendSpeakingAsync(true))
                            let psi =
                                new System.Diagnostics.ProcessStartInfo (
                                    FileName = "ffmpeg.exe",
                                    Arguments = sprintf "-i \"%s\" -ac 2 -f s16le -ar 48000 pipe:1 -loglevel quiet" path,
                                    RedirectStandardOutput = true,
                                    UseShellExecute = false
                                )
                            use ffmpeg = System.Diagnostics.Process.Start psi
                            use ffout = ffmpeg.StandardOutput.BaseStream

                            let txStream = vnc.GetTransmitSink()

                            do! Async.AwaitTask(ffout.CopyToAsync(txStream, cancellationToken=cancelToken.Token))

                            do! Async.AwaitTask(txStream.FlushAsync())
                            do! Async.AwaitTask(vnc.WaitForPlaybackFinishAsync())
                            do! Async.AwaitTask(vnc.SendSpeakingAsync(false))
                            let msg = sprintf "Finished playing %s" path
                            let! _ = Async.AwaitTask(e.Channel.SendMessageAsync(msg))
                            ()
                        with ex ->
                            let msg = sprintf "```%s```" ex.Message
                            let! _ = Async.AwaitTask(e.Channel.SendMessageAsync msg)
                            ()
                    }
                    |> Async.Start

                    { state with
                        CancelationToken = cancelToken }

            | Stop ->
                match vnext.GetConnection(e.Guild) with
                | null ->
                    awaiti <| e.Channel.SendMessageAsync("Not connected in this guild.")

                    state
                | vnc ->
                    let cancelToken =
                        if vnc.IsPlaying then
                            state.CancelationToken.Cancel()

                            new System.Threading.CancellationTokenSource()
                        else
                            state.CancelationToken

                    { state with
                        CancelationToken = cancelToken }

let m =
    let init = {
        CancelationToken = new System.Threading.CancellationTokenSource()
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

    let pplay: _ Parser =
        skipStringCI "play" >>. spaces
        >>. manySatisfy (fun _ -> true)

    let pstop: _ Parser =
        skipStringCI "stop"

    let start =
        choice [
            pjoin >>% Join
            pleave >>% Leave
            pplay |>> Play
            pstop >>% Stop
        ]
