module ChatVoice.Main
open FsharpMyExtension

open Types
open Model

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
                            await (e.Guild.GetMemberAsync e.User.Id)

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
                            await (e.Guild.GetMemberAsync e.User.Id)

                        try
                            channel.AddOverwriteAsync(guildMember, allow=accessChannelsPermission)
                            |> fun x -> x.GetAwaiter() |> fun x -> x.GetResult()
                        with e ->
                            printfn "channel.AddOverwriteAsync: %A" e.Message
                    | None -> ()
    | None -> ()