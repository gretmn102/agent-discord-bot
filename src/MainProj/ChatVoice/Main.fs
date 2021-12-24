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
        do // hide
            match e.Before with
            | null -> ()
            | before ->
                match Map.tryFind before.Channel.Id voiceChats with
                | Some beforeChatId ->
                    let guild = e.Guild
                    let channel = guild.GetChannel beforeChatId
                    let guildMember =
                        try
                            // the error occurs when a person is already in the voice channel, and the bot has just started
                            before.Member
                        with _ ->
                            await (e.Guild.GetMemberAsync e.User.Id)

                    let closedChannelPermissions =
                        LanguagePrimitives.EnumOfValue<int64, DSharpPlus.Permissions>(1071698659905L)

                    try
                        channel.AddOverwriteAsync(guildMember, closedChannelPermissions)
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
                        let guildMember = after.Member

                        let openedChannelPermissions =
                            LanguagePrimitives.EnumOfValue<int64, DSharpPlus.Permissions>(1071698660929L)

                        try
                            channel.AddOverwriteAsync(guildMember, openedChannelPermissions)
                            |> fun x -> x.GetAwaiter() |> fun x -> x.GetResult()
                        with e ->
                            printfn "channel.AddOverwriteAsync: %s" e.Message
                    | None -> ()
    | None -> ()