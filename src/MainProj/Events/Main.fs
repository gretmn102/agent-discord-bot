module Events.Main
open FsharpMyExtension
open FsharpMyExtension.Either
open DSharpPlus

open Types
open Model

type SettingMsg =
    | SetEventSetting of RoleId * bool
    | GetEventSetting

type Msg =
    | SettingMsg of EventArgs.MessageCreateEventArgs * SettingMsg
    | NewMessageHandle of EventArgs.MessageCreateEventArgs

type State = {
    Setting: WomensDaySetting.GuildWomensDaySetting
}

module Parser =
    open FParsec

    open DiscordMessage.Parser

    type 'Result Parser = Primitives.Parser<'Result, unit>

    let pmessage: _ Parser =
        pcodeBlock <|> manySatisfy (fun _ -> true)

    let psetEvent: _ Parser =
        pstringCI "setEvent" >>. spaces
        >>. tuple2
                (puint64 <|> pmentionRole .>> spaces)
                ((pstringCI "true" >>% true) <|> (pstring "false" >>% false))

    let start: _ Parser =
        choice [
            psetEvent |>> SetEventSetting
            pstringCI "getEvent" >>% GetEventSetting
        ]

let settingReduce (e: EventArgs.MessageCreateEventArgs) msg (state: WomensDaySetting.GuildWomensDaySetting) =
    match msg with
    | GetEventSetting ->
        let message =
            match Map.tryFind e.Guild.Id state with
            | Some data ->
                let b = Entities.DiscordMessageBuilder()
                let embed = Entities.DiscordEmbedBuilder()
                embed.Color <- Entities.Optional.FromValue(Entities.DiscordColor("#2f3136"))
                embed.Description <-
                    [
                        "```"
                        Json.ser data
                        "```"
                    ] |> String.concat "\n"

                b.Embed <- embed.Build()
                b
            | None ->
                let b = Entities.DiscordMessageBuilder()
                let embed = Entities.DiscordEmbedBuilder()
                embed.Color <- Entities.Optional.FromValue(Entities.DiscordColor("#2f3136"))
                embed.Description <-
                    [
                        "This server doesn't yet have event setting. To set them, use the command:"
                        "```"
                        ".setEvent <role_mention|role_id> <true|false>"
                        "```"
                    ] |> String.concat "\n"
                b.Embed <- embed.Build()
                b

        awaiti (e.Channel.SendMessageAsync message)

        state
    | SetEventSetting (filteringRoleId, isEnabled) ->
        let guild = e.Guild
        let currentMember = await (guild.GetMemberAsync(e.Author.Id))
        let replyMessage =
            await (e.Channel.SendMessageAsync("Processing..."))

        if currentMember.Permissions &&& Permissions.Administrator = Permissions.Administrator then
            let newData =
                match Map.tryFind guild.Id state with
                | Some setting ->
                    let newData =
                        { setting with
                            FilteringRoleId = filteringRoleId
                            IsEnabled = isEnabled
                        }

                    WomensDaySetting.replace newData

                    newData

                | None ->
                    WomensDaySetting.insert (guild.Id, filteringRoleId, isEnabled)

            let state =
                Map.add guild.Id newData state

            awaiti (replyMessage.ModifyAsync(Entities.Optional "Done"))

            state

        else
            awaiti (replyMessage.ModifyAsync(Entities.Optional "You don't have permission to manage roles"))

            state

let r = System.Random ()
let flowers =
    [|
        "💮"; "🥀"; "💐"; "🌷"; "🌹"; "🥀"; "🌺"; "🌸"; "🌼"; "🌻"; "🏵️"; "🍀"; "🦋"; "🎁"; "🎀"
    |]
    |> Array.map Entities.DiscordEmoji.FromUnicode

let reduce (msg: Msg) (state: State): State =
    match msg with
    | NewMessageHandle e ->
        if e.Author.IsBot then ()
        else
            match Map.tryFind e.Guild.Id state.Setting with
            | Some setting ->
                if setting.IsEnabled then
                    let guildMember = await (e.Guild.GetMemberAsync e.Author.Id)
                    let existsRole =
                        guildMember.Roles
                        |> Seq.exists (fun x -> x.Id = setting.FilteringRoleId)

                    if existsRole then
                        let flower =
                            flowers.[r.Next(0, flowers.Length)]

                        try
                            (e.Message.CreateReactionAsync flower).GetAwaiter().GetResult()
                        with e ->
                            ()
            | None -> ()
        state

    | SettingMsg (e, msg) ->
        { state with
            Setting =
                settingReduce e msg state.Setting
        }

let m =
    let init = {
        Setting = WomensDaySetting.getAll ()
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

let exec e msg =
    m.Post (SettingMsg (e, msg))
