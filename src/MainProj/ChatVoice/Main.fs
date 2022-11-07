module ChatVoice.Main
open FsharpMyExtension
open FsharpMyExtension.Either
open DSharpPlus

open Types
open Model

type Request =
    | MentionInVoice

module Parser =
    open FParsec

    open DiscordMessage.Parser

    type 'a Parser = Parser<'a, unit>

    let start f: _ Parser =
        skipStringCI "inVoice" >>% MentionInVoice
        >>= fun msg ->
            preturn (fun x -> f x msg)

let settings: Settings =
    [
        878956153537691658UL, Map.ofList [
            878956154049429596UL, 923840399297085460UL
            923840636266876960UL, 923840590251184130UL
        ]
        914459693924110356UL, Map.ofList [
            914459694758772781UL, 923846477300781056UL
            914459694758772782UL, 915200369687167017UL
        ]
    ]
    |> Map.ofList

let voiceHandle (e: DSharpPlus.EventArgs.VoiceStateUpdateEventArgs) =
    match Map.tryFind e.Guild.Id settings with
    | Some voiceChats ->
        let accessChannelsPermission =
            DSharpPlus.Permissions.AccessChannels

        do // hide
            match e.Before with
            | null -> ()
            | before ->
                match before.Channel with
                | null -> ()
                | channel ->
                    match Map.tryFind channel.Id voiceChats with
                    | Some beforeChatId ->
                        let guild = e.Guild
                        let channel = guild.GetChannel beforeChatId
                        let guildMember =
                            getGuildMember e.Guild e.User

                        try
                            channel.AddOverwriteAsync(guildMember, deny=accessChannelsPermission)
                            |> fun x -> x.GetAwaiter() |> fun x -> x.GetResult()
                        with e ->
                            printfn "channel.AddOverwriteAsync: %s" e.Message
                    | None -> ()

        do // show
            match e.After with
            | null -> ()
            | after ->
                match after.Channel with
                | null -> ()
                | channel ->
                    match Map.tryFind channel.Id voiceChats with
                    | Some beforeChatId ->
                        let guild = e.Guild
                        let channel = guild.GetChannel beforeChatId
                        let guildMember =
                            getGuildMember e.Guild e.User

                        try
                            channel.AddOverwriteAsync(guildMember, allow=accessChannelsPermission)
                            |> fun x -> x.GetAwaiter() |> fun x -> x.GetResult()
                        with e ->
                            printfn "channel.AddOverwriteAsync: %A" e.Message
                    | None -> ()
    | None -> ()

let reduce (e: DSharpPlus.EventArgs.MessageCreateEventArgs) msg =
    match msg with
    | MentionInVoice ->
        let guildMember = getGuildMember e.Guild e.Author
        let errorMessage =
            "This command is available only if you are in the voice channel of this guild."
        match guildMember.VoiceState with
        | null ->
            awaiti (e.Channel.SendMessageAsync errorMessage)
        | vs ->
            match vs.Channel with
            | null ->
                awaiti (e.Channel.SendMessageAsync errorMessage)
            | channel ->
                if vs.Guild.Id = e.Guild.Id then
                    let users =
                        channel.Users
                        |> Seq.choose (fun x ->
                            if x.Id = guildMember.Id then None
                            else Some (sprintf "<@!%d>" x.Id)
                        )
                    let msg =
                        if Seq.isEmpty users then
                            "You are the only one in the voice channel."
                        else
                            users
                            |> String.concat ", "

                    awaiti (e.Channel.SendMessageAsync msg)
                else
                    awaiti (e.Channel.SendMessageAsync errorMessage)

let exec: MessageCreateEventHandler Parser.Parser =
    Parser.start (fun (client: DiscordClient, e: EventArgs.MessageCreateEventArgs) msg ->
        reduce e msg
    )
