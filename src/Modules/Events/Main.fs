module Events.Main
open FsharpMyExtension
open FsharpMyExtension.Either
open DSharpPlus

open Types
open Extensions

type SettingMsg =
    | SetEventSetting of RoleId list * bool
    | GetEventSetting

type Msg =
    | SettingMsg of EventArgs.MessageCreateEventArgs * SettingMsg
    | NewMessageHandle of EventArgs.MessageCreateEventArgs

type State = {
    Setting: Model.Guilds
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
                (many1 ((puint64 <|> pmentionRole) .>> spaces))
                ((pstringCI "true" >>% true) <|> (pstring "false" >>% false))

    let start f: _ Parser =
        choice [
            psetEvent |>> SetEventSetting
            pstringCI "getEvent" >>% GetEventSetting
        ]
        >>= fun msg ->
            preturn (fun x -> f x msg)

let settingReduce (e: EventArgs.MessageCreateEventArgs) msg (state: Model.Guilds) =
    match msg with
    | GetEventSetting ->
        let message =
            match Model.Guilds.tryFindById e.Guild.Id state with
            | Some data ->
                let b = Entities.DiscordMessageBuilder()
                let embed = Entities.DiscordEmbedBuilder()
                embed.Color <- Entities.Optional.FromValue(DiscordEmbed.backgroundColorDarkTheme)
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
                embed.Color <- Entities.Optional.FromValue(DiscordEmbed.backgroundColorDarkTheme)
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
        let currentMember = getGuildMember guild e.Author
        let replyMessage =
            await (e.Channel.SendMessageAsync("Processing..."))

        if currentMember.Permissions &&& Permissions.Administrator = Permissions.Administrator then
            let settings =
                state
                |> Model.Guilds.set
                    guild.Id
                    (fun settings ->
                        { settings with
                            FilteringRoleId = Set.ofList filteringRoleId
                            IsEnabled = isEnabled
                        }
                    )

            awaiti (replyMessage.ModifyAsync(Entities.Optional "Done"))

            settings

        else
            awaiti (replyMessage.ModifyAsync(Entities.Optional "You don't have permission to manage roles"))

            state

let r = System.Random ()
let flowers =
    [|
        // TODO: formalize
        // "💮"; "🥀"; "💐"; "🌷"; "🌹"; "🥀"; "🌺"; "🌸"; "🌼"; "🌻"; "🏵️"; "🍀"; "🦋"; "🎁"; "🎀"
        "🎅"; "🐇"; "❄️"; "☃️"; "⛄"; "🎉"; "🎁"; "🎄"; "🥂"; "💝"; "✨"; "🎈"
    |]
    |> Array.map Entities.DiscordEmoji.FromUnicode

let reduce (msg: Msg) (state: State): State =
    match msg with
    | NewMessageHandle e ->
        if e.Author.IsBot then ()
        else
            match Model.Guilds.tryFindById e.Guild.Id state.Setting with
            | Some setting ->
                if setting.Data.IsEnabled then
                    let guildMember = getGuildMember e.Guild e.Author
                    let existsRole =
                        guildMember.Roles
                        |> Seq.exists (fun x -> Set.contains x.Id setting.Data.FilteringRoleId)

                    if existsRole then
                        let flower =
                            flowers.[r.Next(0, flowers.Length)]

                        try
                            awaiti <| e.Message.CreateReactionAsync flower
                        with e ->
                            ()
            | None -> ()
        state

    | SettingMsg (e, msg) ->
        { state with
            Setting =
                settingReduce e msg state.Setting
        }

let create db =
    let m =
        let init = {
            Setting = Model.Guilds.init "womensDaySetting" db
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

    { Shared.BotModule.empty with
        MessageCreateEventHandleExclude =
            let exec: _ Parser.Parser =
                Parser.start (fun (client: DiscordClient, e: EventArgs.MessageCreateEventArgs) msg ->
                    m.Post (SettingMsg (e, msg))
                )
            Some exec

        MessageCreateEventHandle =
            let handle (client, (e: EventArgs.MessageCreateEventArgs)) =
                m.Post (NewMessageHandle e)
            Some handle
    }
