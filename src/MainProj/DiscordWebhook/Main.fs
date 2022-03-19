module DiscordWebhook.Main
open FsharpMyExtension
open FsharpMyExtension.Either
open DSharpPlus

open Types
open Model

type Profile = { Username: string; AvatarUrl: string }

type SettingReq =
    | SetWebhookId of WebhookId
    | SetCharacter of Profile

type ProfileMode =
    | ManualProfile of Profile
    | UserProfile of UserId

type Request =
    | Send of {| ChannelId: ChannelId; ProfileMode: ProfileMode; Content: string |}
    | SendByCharacter of content: string
    | SettingReq of SettingReq

module Parser =
    open FParsec

    open DiscordMessage.Parser

    type 'a Parser = Parser<'a, unit>

    let purl: _ Parser =
        pipe2
            (pstring "http")
            (manySatisfy (not << System.Char.IsWhiteSpace))
            (sprintf "%s%s")

    let psend: _ Parser =
        let pmanualProfile =
            pipe2
                (pquote .>> spaces)
                (purl .>> spaces)
                (fun userName url ->
                    { Username = userName; AvatarUrl = url }
                )

        skipStringCI "sendHook" .>> spaces
        >>. pipe3
                (pchannelMention .>> spaces)
                ((pmanualProfile |>> ManualProfile)
                 <|> ((puint64 <|> puserMention) |>> UserProfile))
                (manySatisfy (fun _ -> true))
                (fun channelId profile msg ->
                    {|
                        ChannelId = channelId
                        ProfileMode = profile
                        Content = msg
                    |}
                )

    let psendByCharacter: _ Parser =
        skipStringCI "say" .>> spaces
        >>. (manySatisfy (fun _ -> true))

    let setWebhookName = "setWebhook"
    let setCharacterName = "setCharacter"

    let psetting: _ Parser =
        let psetWebhook =
            // "https://discord.com/api/webhooks/%id%/%token%
            let pwebhookUrl =
                skipStringCI "https://discord.com/api/webhooks/"
                >>. puint64

            pstring setWebhookName .>> spaces
            >>. (puint64 <|> pwebhookUrl)

        let setCharacter =
            skipStringCI setCharacterName .>> spaces
            >>. pipe2
                    (pquote .>> spaces)
                    (purl .>> spaces)
                    (fun userName url ->
                        { AvatarUrl = url; Username = userName })
        choice [
           psetWebhook |>> SetWebhookId
           setCharacter |>> SetCharacter
        ]

    let start: _ Parser =
        choice [
            psendByCharacter |>> SendByCharacter
            psend |>> Send
            psetting |>> SettingReq
        ]

type State =
    {
        TargetWebhooks: TargetWebhooks.GuildTargetWebhook
        Characters: Characters.GuildCharacters
    }

let reduce (e: EventArgs.MessageCreateEventArgs) msg (state: State) =
    match msg with
    | Send opts ->
        let currentMember = await (e.Guild.GetMemberAsync(e.Author.Id))
        let replyMessage =
            await (e.Channel.SendMessageAsync "Processing...")

        let msg =
            match e.Guild.GetChannel opts.ChannelId with
            | null ->
                sprintf "Channel %d not found" opts.ChannelId

            | channel ->
                if currentMember.Permissions &&& Permissions.Administrator = Permissions.Administrator then
                    let webhooks = await (channel.GetWebhooksAsync())

                    let webhook =
                        if webhooks.Count > 0 then
                            Right webhooks.[0]
                        else
                            try
                                await (channel.CreateWebhookAsync(sprintf "%d webhook" opts.ChannelId))
                                |> Right
                            with e ->
                                [
                                    "Error:"
                                    "```"
                                    e.Message
                                    "```"
                                ] |> String.concat "\n"
                                |> Left

                    match webhook with
                    | Right webhook ->
                        let profile =
                            match opts.ProfileMode with
                            | ManualProfile profile -> Right profile
                            | UserProfile userId ->
                                // let client: DiscordClient = ()
                                // match await (client.GetUserAsync userId) with
                                // | null ->
                                //     Left (sprintf "User %d is not found" userId)
                                // | user ->
                                //     Right {| AvatarUrl = user.AvatarUrl; Username = user.Username |}

                                match await (e.Guild.GetMemberAsync userId) with
                                | null ->
                                    Left (sprintf "Member %d is not found" userId)
                                | user ->
                                    let userName =
                                        if System.String.IsNullOrWhiteSpace user.Nickname then
                                            user.Username
                                        else
                                            user.Nickname
                                    Right { AvatarUrl = user.AvatarUrl; Username = userName }

                        match profile with
                        | Right profile ->
                            let b = Entities.DiscordWebhookBuilder()

                            b.WithUsername profile.Username |> ignore
                            b.WithAvatarUrl profile.AvatarUrl |> ignore

                            b.WithContent opts.Content |> ignore

                            try
                                awaiti (webhook.ExecuteAsync b)

                                "Done"
                            with e ->
                                [
                                    "Error:"
                                    "```"
                                    e.Message
                                    "```"
                                ] |> String.concat "\n"
                        | Left errMsg -> errMsg
                    | Left errMsg -> errMsg

                else
                    "You don't have administration permission"

        awaiti (replyMessage.ModifyAsync(Entities.Optional msg))

        state
    | SendByCharacter content ->
        match Map.tryFind e.Guild.Id state.TargetWebhooks with
        | Some targetWebhookSetting ->
            let errorMessage =
                [
                    sprintf "Webhook by %d id will not find. To set new webhook, use:" targetWebhookSetting.WebhookId
                    "```"
                    sprintf ".%s 1234567" Parser.setWebhookName
                    "```"
                    "Where 1234567 — webhook id."
                ] |> String.concat "\n"

            match Map.tryFind e.Guild.Id state.Characters with
            | Some guildCharacters ->
                match Map.tryFind e.Author.Id guildCharacters with
                | Some character ->
                    let webhooks = await (e.Guild.GetWebhooksAsync())
                    let targetWebhook =
                        webhooks
                        |> Seq.tryFind (fun x -> x.Id = targetWebhookSetting.WebhookId)

                    match targetWebhook with
                    | Some targetWebhook ->
                        let b = Entities.DiscordWebhookBuilder()

                        b.WithUsername character.Username |> ignore
                        b.WithAvatarUrl character.AvatarUrl |> ignore
                        b.WithContent content |> ignore

                        awaiti (targetWebhook.ExecuteAsync b)
                    | None ->
                        awaiti (e.Channel.SendMessageAsync errorMessage)
                | None ->
                    awaiti (e.Channel.SendMessageAsync errorMessage)

            | None ->
                let msg =
                    [
                        sprintf "You haven't created a character yet. To create a character, use the `.%s` command." Parser.setCharacterName
                        "For example:"
                        "```"
                        sprintf ".%s \"Captain hook\" https://i2.wp.com/static4.wikia.nocookie.net/__cb20130519181351/disney/images/a/ac/Peter-pan-disneyscreencaps.com-7911.jpg" Parser.setCharacterName
                        "```"
                    ] |> String.concat "\n"

                awaiti (e.Channel.SendMessageAsync msg)
        | None ->
            let msg =
                [
                    sprintf "Admin has not yet set webhook to use this command. To set webhook, use:"
                    "```"
                    sprintf ".%s 1234567" Parser.setWebhookName
                    "```"
                    "Where 1234567 — webhook id."
                ] |> String.concat "\n"

            awaiti (e.Channel.SendMessageAsync msg)

        state
    | SettingReq settingReq ->
        let currentMember = await (e.Guild.GetMemberAsync(e.Author.Id))
        let replyMessage =
            await (e.Channel.SendMessageAsync("Processing..."))

        match settingReq with
        | SetWebhookId webhookId ->
            if currentMember.Permissions &&& Permissions.Administrator = Permissions.Administrator then
                let targetWebhookSetting =
                    match Map.tryFind e.Guild.Id state.TargetWebhooks with
                    | Some targetWebhookSetting ->
                        let targetWebhookSetting =
                            { targetWebhookSetting with WebhookId = webhookId }

                        TargetWebhooks.replace targetWebhookSetting

                        targetWebhookSetting
                    | None ->
                        TargetWebhooks.insert(e.Guild.Id, webhookId)

                let state =
                    { state with
                        TargetWebhooks =
                            Map.add e.Guild.Id targetWebhookSetting state.TargetWebhooks
                    }

                awaiti (replyMessage.ModifyAsync(Entities.Optional "Done!"))

                state
            else
                awaiti (replyMessage.ModifyAsync(Entities.Optional "You don't have administration permission"))

                state

        | SetCharacter character ->
            let state =
                match Map.tryFind e.Guild.Id state.Characters with
                | Some guildCharacters ->
                    let character =
                        match Map.tryFind e.Author.Id guildCharacters with
                        | Some characterData ->
                            let character =
                                { characterData with
                                    AvatarUrl = character.AvatarUrl
                                    Username = character.Username
                                }

                            Characters.replace character

                            character
                        | None ->
                            Characters.insert(e.Guild.Id, e.Author.Id, character.Username, character.AvatarUrl)

                    { state with
                        Characters =
                            let guildCharacters = Map.add e.Author.Id character guildCharacters
                            Map.add e.Guild.Id guildCharacters state.Characters
                    }

                | None ->
                    let character = Characters.insert(e.Guild.Id, e.Author.Id, character.Username, character.AvatarUrl)

                    { state with
                        Characters =
                            let guildCharacters = Map.add e.Author.Id character Map.empty
                            Map.add e.Guild.Id guildCharacters state.Characters
                    }

            awaiti (replyMessage.ModifyAsync(Entities.Optional "Done!"))

            state


type Req = EventArgs.MessageCreateEventArgs * Request

let m: MailboxProcessor<Req> =
    let init = {
        TargetWebhooks = TargetWebhooks.getAll ()
        Characters = Characters.getAll ()
    }

    MailboxProcessor.Start (fun mail ->
        let rec loop (state: State) =
            async {
                let! (e, msg) = mail.Receive()
                let state =
                    try
                        reduce e msg state
                    with e ->
                        printfn "%A" e
                        state

                return! loop state
            }
        loop init
    )

let exec e msg =
    m.Post(e, msg)
