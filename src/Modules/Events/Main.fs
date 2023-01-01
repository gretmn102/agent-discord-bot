module Events.Main
open FsharpMyExtension
open FsharpMyExtension.Either
open DSharpPlus

open Types
open Extensions
open DiscordMessage

type SettingMsg =
    | SetEventSetting of Model.GuildData
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
        let pbool =
            (pstringCI "true" >>% true) <|> (pstring "false" >>% false)

        pstringCI "setEvent" >>. spaces
        >>. pipe3
                (many1 ((puint64 <|> pmentionRole) .>> spaces))
                (pbool .>> spaces)
                (many1 (pemoji .>> spaces))
                (fun roleIds isEnabled unicodeOrCustomEmojis ->
                    let emojis =
                        unicodeOrCustomEmojis
                        |> List.map (fun unicodeOrCustomEmoji ->
                            match unicodeOrCustomEmoji with
                            | UnicodeOrCustomEmoji.CustomEmoji x ->
                                sprintf "<:%s:%d>" x.Name x.Id

                            | UnicodeOrCustomEmoji.UnicodeEmoji emoji ->
                                emoji
                        )
                        |> Array.ofList

                    Model.GuildData.Init
                        (Set.ofList roleIds)
                        isEnabled
                        emojis
                )

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
    | SetEventSetting newParams ->
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
                        newParams
                    )

            awaiti (replyMessage.ModifyAsync(Entities.Optional "Done"))

            settings

        else
            awaiti (replyMessage.ModifyAsync(Entities.Optional "You don't have permission to manage roles"))

            state

let r = System.Random ()

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
                        let emojis =
                            setting.Data.Emojis

                        if not <| Array.isEmpty emojis then
                            let parseEmoji rawEmoji next =
                                match FParsecExt.runResult Parser.pemoji rawEmoji with
                                | Ok x -> next x
                                | Error _ -> ()

                            let getUnicodeOrCustomEmoji unicodeOrCustomEmoji next =
                                match unicodeOrCustomEmoji with
                                | UnicodeOrCustomEmoji.CustomEmoji x ->
                                    let getEmoji (emojiId: EmojiId) =
                                        try
                                            await <| e.Guild.GetEmojiAsync emojiId
                                            |> Ok
                                        with e ->
                                            // TODO: handle disconnect
                                            Error "Такого эмодзи не существует."

                                    match getEmoji x.Id with
                                    | Ok emoji -> next (emoji :> Entities.DiscordEmoji)
                                    | _ -> ()

                                | UnicodeOrCustomEmoji.UnicodeEmoji emoji ->
                                    Entities.DiscordEmoji.FromUnicode emoji
                                    |> next

                            let rawEmoji =
                                emojis.[r.Next(0, emojis.Length)]

                            parseEmoji rawEmoji <| fun unicodeOrCustomEmoji ->
                            getUnicodeOrCustomEmoji unicodeOrCustomEmoji <| fun emoji ->

                            try
                                awaiti <| e.Message.CreateReactionAsync emoji
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
