module App
open FsharpMyExtension
open FsharpMyExtension.Either
open Microsoft.Extensions.Logging
open System.Threading.Tasks
open DSharpPlus.VoiceNext

open Types
open Svg

let botEventId = new EventId(42, "Bot-Event")

let r = System.Random()
let gifUrls =
    [|
        "https://c.tenor.com/ccxh_NDfHXMAAAAC/carry-her-lift.gif"
        "https://c.tenor.com/ytkH6MSlE4EAAAAC/love-relationship.gif"
        "https://c.tenor.com/TFleYTQhTCEAAAAC/arm-carry-cute.gif"
        "https://c.tenor.com/OulTpS0kWYMAAAAC/madison-beer.gif"
        "https://c.tenor.com/CMD-o3NJdV4AAAAC/wonder-woman-lovers-carry.gif"
        "https://c.tenor.com/3Qmu-zdjFIwAAAAC/carry-shoulder.gif"
        "https://c.tenor.com/ydSMRf34XvEAAAAC/spin-carry.gif"
    |]

let fairytailGifs =
    [|
        "https://c.tenor.com/Lj5SFh_tVzkAAAAC/books-read.gif"
        "https://c.tenor.com/Vucu5O0U4FAAAAAC/cat-kitten.gif"
        "https://c.tenor.com/415n58OZ9CYAAAAd/cat-reads-reading.gif"
        "https://c.tenor.com/SajtOuJknfYAAAAd/cute-cat.gif"
        "https://c.tenor.com/415n58OZ9CYAAAAd/cat-reads-reading.gif"
        "https://c.tenor.com/dtAQHKf2_OsAAAAC/pusheen-witch.gif"
        "https://c.tenor.com/2hatW6KUSS8AAAAC/reading-read.gif"
        "https://c.tenor.com/Yw68weAL6c0AAAAC/knigi-kniga.gif"
    |]

let catailGifs =
    [|
        "https://c.tenor.com/8yvB03LKh6cAAAAd/wow-cat.gif"
        "https://c.tenor.com/_SHZ8ZyLYL8AAAAC/flirty-flirt.gif"
        "https://c.tenor.com/bNSOiEO_0loAAAAd/cat-tail.gif"
        "https://c.tenor.com/TnXmJgMoU5IAAAAC/cat-tail.gif"
        "https://c.tenor.com/kWJaRBz4jzYAAAAC/elephant-omg.gif"
    |]

let admireGifs =
    [|
        "https://risovach.ru/upload/2012/11/lyubov-4219820_orig_.jpeg"
        "https://i.imgur.com/ArtzYH0.jpg"
    |]

let batteryGifs =
    [|
        "https://c.tenor.com/X45Rd5nfc7oAAAAM/energize-energized.gif"
    |]

let catchAssets =
    [|
        "https://cdn.discordapp.com/attachments/864883475386990664/895218342141509632/Screenshot_20181219-151451_1.jpg"
    |]

let angryAssets =
    [|
        "https://c.tenor.com/ikKAd57zDEwAAAAd/anime-mad.gif"
        "https://c.tenor.com/rzDkOlEDun0AAAAC/hayase-nagatoro-nagatoro-angry.gif"
        "https://c.tenor.com/kTOmea7XdH4AAAAC/anime-angry.gif"
        "https://c.tenor.com/7rIJkf8pB2EAAAAd/a-channel-tooru.gif"
        "https://c.tenor.com/NxIgKHx6bu0AAAAC/glare-anger.gif"
        "https://c.tenor.com/0YwfR1RlISEAAAAC/angry-fist-angry-fist-arthur.gif"
        "https://c.tenor.com/Hn87zvoPNkAAAAAC/fire-bear.gif"
    |]

let cmd (client:DSharpPlus.DiscordClient) (e:DSharpPlus.EventArgs.MessageCreateEventArgs) =
    let authorId = e.Author.Id
    let botId = client.CurrentUser.Id

    let cmdBuilder2 (opt:CommandParser.ShipOption) usersIds (whomAuthorPhrase:string) (whomBotPhrase:string) =
        let send (whoId:UserId) (whomId:UserId) =
            // let who = e.Guild.Members.[whoId] // fired "The given key '...' was not present in the dictionary."
            // let whom = e.Guild.Members.[whomId]
            let who = await (e.Guild.GetMemberAsync whoId)
            let whom = await (e.Guild.GetMemberAsync whomId)

            let fileName = "heart.png"

            let b = DSharpPlus.Entities.DiscordMessageBuilder()

            let perc =
                match opt with
                | CommandParser.Target x -> x
                | CommandParser.Rand -> r.Next(0, 101)

            let embed = DSharpPlus.Entities.DiscordEmbedBuilder()
            embed.Color <- DSharpPlus.Entities.Optional.FromValue(DSharpPlus.Entities.DiscordColor("#2f3136"))
            embed.Description <-
                let nickOrName (user:DSharpPlus.Entities.DiscordMember) =
                    match user.Nickname with
                    | null -> user.Username
                    | nick -> nick

                if perc < 50 then
                    sprintf "Между %s и %s..." (nickOrName who) (nickOrName whom)
                else
                    sprintf "Между %s и %s что-то есть!" (nickOrName who) (nickOrName whom)

            embed.WithImageUrl (sprintf "attachment://%s" fileName) |> ignore
            b.Embed <- embed.Build()

            let user1Avatar = WebClientDownloader.getData [] who.AvatarUrl
            let user2Avatar = WebClientDownloader.getData [] whom.AvatarUrl

            use m = new System.IO.MemoryStream()
            Ship.img user1Avatar user2Avatar perc m
            m.Position <- 0L
            b.WithFile(fileName, m) |> ignore

            awaiti (client.SendMessageAsync (e.Channel, b))
        match usersIds with
        | [] ->
            match e.Message.ReferencedMessage with
            | null ->
                awaiti (client.SendMessageAsync (e.Channel, "Нужно указать пользователя"))
            | referencedMessage ->
                let whomId = referencedMessage.Author.Id
                if whomId = authorId then
                    awaiti (client.SendMessageAsync (e.Channel, whomAuthorPhrase))
                elif whomId = botId then
                    awaiti (client.SendMessageAsync (e.Channel, whomBotPhrase))
                else
                    send e.Author.Id whomId
        | [whomId] ->
            if whomId = authorId then
                awaiti (client.SendMessageAsync (e.Channel, whomAuthorPhrase))
            elif whomId = botId then
                awaiti (client.SendMessageAsync (e.Channel, whomBotPhrase))
            else
                send authorId whomId
        | whoId::whomId::_ ->
            if whomId = whoId then
                awaiti (client.SendMessageAsync (e.Channel, whomAuthorPhrase))
            elif whoId = botId || whomId = botId then
                awaiti (client.SendMessageAsync (e.Channel, whomBotPhrase))
            else
                send whoId whomId


    let cmdBuilder whomId (gifs: string []) content (whomAuthorPhrase:string) (whomBotPhrase:string) =
        let send whomId =
            let whom =
                client.GetUserAsync whomId
                |> fun x -> x.GetAwaiter().GetResult()

            let b = DSharpPlus.Entities.DiscordEmbedBuilder()
            b.Description <- content e.Author.Username whom.Username

            b.Color <- DSharpPlus.Entities.Optional.FromValue(DSharpPlus.Entities.DiscordColor("#2f3136"))
            if not (Array.isEmpty gifs) then
                b.ImageUrl <- gifs.[r.Next(0, gifs.Length)]

            awaiti (client.SendMessageAsync (e.Channel, b.Build()))
        match whomId with
        | Some whomId ->
            if whomId = authorId then
                awaiti (client.SendMessageAsync (e.Channel, whomAuthorPhrase))
            elif whomId = botId then
                awaiti (client.SendMessageAsync (e.Channel, whomBotPhrase))
            else
                send whomId
        | None ->
            match e.Message.ReferencedMessage with
            | null ->
                awaiti (client.SendMessageAsync (e.Channel, "Нужно указать пользователя"))
            | referencedMessage ->
                send referencedMessage.Author.Id

    if authorId <> botId then
        match CommandParser.start botId e.Message.Content with
        | Right res ->
            match res with
            | CommandParser.Pass -> ()
            | CommandParser.Act(act, whomId) ->
                match act with
                | CommandParser.Take ->
                    cmdBuilder
                        whomId
                        gifUrls
                        (sprintf "**%s** носит на ручках **%s**")
                        "Самого себя нельзя на руках носить :eyes:"
                        "Меня не нужно носить! :scream_cat: "
                | CommandParser.Fairytail ->
                    cmdBuilder
                        whomId
                        fairytailGifs
                        (sprintf "**%s** читает сказку **%s**")
                        "Нельзя себе сказку читать :eyes:"
                        "Мне не нужно сказки читать! :scream_cat: "
                | CommandParser.Catail ->
                    cmdBuilder
                        whomId
                        catailGifs
                        (fun who whom -> sprintf "**%s**, **%s** машет тебе хвостом" whom who)
                        "Нельзя себе хвостом махать, хотя..."
                        "Мне не нужно хвостом махать! :scream_cat: "
                | CommandParser.Bully ->
                    cmdBuilder
                        whomId
                        [||]
                        (sprintf "**%s** буллит **%s** <:Demon_Kingsmile:877678191693692969>")
                        "Себя нельзя буллить! Хотя..."
                        "Мне нельзя буллить! :scream_cat: "
                | CommandParser.Admire ->
                    cmdBuilder
                        whomId
                        admireGifs
                        (sprintf "**%s** любуется **%s**")
                        "Нельзя любоваться собой :eyes:"
                        "Нельзя мною любоваться :scream_cat: "
                | CommandParser.Battery ->
                    cmdBuilder
                        whomId
                        batteryGifs
                        (sprintf "**%s** поет \"Батарейку\" **%s**")
                        "Самому нельзя петь \"Батерей\"!"
                        "Мне нельзя петь \"Батарейку\". Я этого не вынесу :scream_cat: "
                | CommandParser.Ship opt ->
                    cmdBuilder2
                        opt
                        [ match whomId with Some x -> x | None -> () ]
                        "Нельзя с самим собой шипериться!"
                        "Нельзя со мной шипериться! :scream_cat: "
                | CommandParser.Catch ->
                    cmdBuilder
                        whomId
                        catchAssets
                        (sprintf "**%s** ловит **%s**")
                        "Самого нельзя ловить!"
                        "Меня нельзя ловить! Я этого не вынесу :scream_cat: "
                | CommandParser.Angry ->
                    cmdBuilder
                        whomId
                        angryAssets
                        (sprintf "**%s** злится на **%s**")
                        "На самого себя нельзя злиться, ну в самом деле!"
                        "На меня нельзя злиться! Я хороший"
            | CommandParser.Cyoa x ->
                AppsHub.start (AppsHub.Hub.InitCyoa x) client e
            | CommandParser.SomeQuiz ->
                AppsHub.start AppsHub.Hub.InitQuiz client e
            | CommandParser.BallotBox(description, choices) ->
                AppsHub.start (AppsHub.Hub.InitBallotBox(description, choices)) client e
            | CommandParser.MassShip usersIds ->
                let f (msg:DSharpPlus.Entities.DiscordMessage) =
                    async {
                        let usersAvatars =
                            usersIds
                            |> Seq.map (fun userId ->
                                let user = await (e.Guild.GetMemberAsync userId)
                                WebClientDownloader.getData [] user.AvatarUrl
                            )
                            |> Seq.map (function
                                | Right xs -> xs
                                | Left _ -> [||]
                            )
                            |> Array.ofSeq

                        let b = DSharpPlus.Entities.DiscordMessageBuilder()
                        // let embed = DSharpPlus.Entities.DiscordEmbedBuilder()
                        // embed.Color <- DSharpPlus.Entities.Optional.FromValue(DSharpPlus.Entities.DiscordColor("#2f3136"))
                        // embed.Description <-
                        //     let nickOrName (user:DSharpPlus.Entities.DiscordMember) =
                        //         match user.Nickname with
                        //         | null -> user.Username
                        //         | nick -> nick

                        //     if perc < 50 then
                        //         sprintf "Между %s и %s..." (nickOrName who) (nickOrName whom)
                        //     else
                        //         sprintf "Между %s и %s что-то есть!" (nickOrName who) (nickOrName whom)
                        let fileName = "massShip.png"
                        // embed.WithImageUrl (sprintf "attachment://%s" fileName) |> ignore

                        // b.Embed <- embed.Build()

                        use m = new System.IO.MemoryStream()
                        Ship.massShip usersAvatars m
                        m.Position <- 0L
                        b.WithFile(fileName, m) |> ignore
                        let! _ = Async.AwaitTask (msg.ModifyAsync(b))
                        ()
                    }

                let msg = await (client.SendMessageAsync (e.Channel, "Processing..."))
                Async.Start (f msg)

            | CommandParser.Role r ->
                Role.Main.giveOrChangeRole e r
            | CommandParser.AddPermissiveRole r ->
                Role.Main.addPermisiveRole e r
            | CommandParser.RemovePermissiveRole r ->
                Role.Main.removePermisiveRole e r
            | CommandParser.GetPermissiveRoles ->
                Role.Main.getPermisiveRole e
            | CommandParser.GetUserRoles ->
                Role.Main.getUserRoles e
            | CommandParser.RemoveUserRole userRole ->
                Role.Main.removeUserRole e userRole
            | CommandParser.SetTemplateRole userRole ->
                Role.Main.setTemplateRole e userRole
            | CommandParser.UpdateUserRolesPermissions ->
                Role.Main.updateRolesPermission e

            | CommandParser.Doorkeeper newcomersRolesMsg ->
                Doorkeeper.Main.execNewcomersRolesCmd e newcomersRolesMsg

            | CommandParser.VoiceChannelNotification msg ->
                VoiceChannelNotification.Main.execVoiceNotificationCmd e msg

            | CommandParser.MusicCmd msg ->
                Music.Main.exec client e msg

            | CommandParser.RankingCmd msg ->
                Ranking.Main.execSettingCmd e msg

            | CommandParser.MessageManagerCmd msg ->
                MessageManager.exec client e msg

            | CommandParser.Unknown ->
                let b = DSharpPlus.Entities.DiscordEmbedBuilder()
                b.Description <-
                    [
                        "Неизвестная команда. Доступные команды:"
                        "`.take @user` — взять кого-то на ручки"
                        "`.fairytail @user` — почитать кому-то сказку"
                        "`.catail @user` — помахать кому-то хвостом"
                        "`.bully @user` — забуллить кого-то <:Demon_Kingsmile:877678191693692969>"
                        "`.admire @user` — любоваться"
                        "`.battery` — спеть \"Батарейку\""
                        "`.catch @user` — поймать кого-то"
                        "`.angry @user` — разозлиться на кого-то"
                        "`.numberToWords <число>` — говорит число словами, например, `.numberToWords 21435` выдаст:"
                        "```"
                        "двадцать одна тысяча четыреста тридцать пять```"
                    ] |> String.concat "\n"

                b.Color <- DSharpPlus.Entities.Optional.FromValue(DSharpPlus.Entities.DiscordColor("#2f3136"))
                awaiti (client.SendMessageAsync (e.Channel, b.Build()))
            | CommandParser.NumberToWords num ->
                let b = DSharpPlus.Entities.DiscordEmbedBuilder()
                b.Description <-
                    try
                        NumberToWords.toNumName num
                    with e ->
                        e.Message
                b.Color <- DSharpPlus.Entities.Optional.FromValue(DSharpPlus.Entities.DiscordColor("#2f3136"))
                awaiti (client.SendMessageAsync (e.Channel, b.Build()))
            | CommandParser.EmojiFont (emoji, str) ->
                let emojiFont emojiImg =
                    use m = new System.IO.MemoryStream()
                    EmojiFont.drawText emojiImg str m
                    m.Position <- 0L

                    let b = DSharpPlus.Entities.DiscordMessageBuilder()
                    b.WithFile("image.png", m) |> ignore

                    awaiti (client.SendMessageAsync (e.Channel, b))
                let emojiImgWidth = 32
                match emoji with
                | CommandParser.CustomEmoji emoji ->
                    let emojiSrc = sprintf "https://cdn.discordapp.com/emojis/%d.png?size=%d" emoji.Id emojiImgWidth
                    let emojiImg = WebClientDownloader.getData [] emojiSrc
                    emojiFont emojiImg
                | CommandParser.UnicodeEmoji emoji ->
                    match StandartDiscordEmoji.getEmojiSheet () with
                    | Right emojiSheet ->
                        use m = new System.IO.MemoryStream()
                        if StandartDiscordEmoji.getEmoji emojiSheet emoji m then
                            m.ToArray()
                            |> Right
                            |> emojiFont
                        else
                            awaiti (client.SendMessageAsync (e.Channel, sprintf "\"%s\" — этот emoji пока что не поддерживается." emoji))
                    | Left errMsg ->
                        emojiFont (Left errMsg)
        | Left x ->
            awaiti (client.SendMessageAsync (e.Channel, (sprintf "Ошибка:\n```\n%s\n```" x)))

[<EntryPoint>]
let main argv =
    let tokenEnvVar =
        #if TEST_BOT
        "TestBotToken"
        #else
        "DiscordCommandBotToken"
        #endif

    match System.Environment.GetEnvironmentVariable tokenEnvVar with
    | null ->
        printfn "Environment variable `%s` not setup" tokenEnvVar
        1
    | token ->
        let config = DSharpPlus.DiscordConfiguration()

        config.set_Token token
        config.set_TokenType DSharpPlus.TokenType.Bot
        config.set_AutoReconnect true
        config.set_Intents (DSharpPlus.DiscordIntents.AllUnprivileged ||| DSharpPlus.DiscordIntents.GuildMembers)

        let client = new DSharpPlus.DiscordClient(config)

        let voice = client.UseVoiceNext()

        client.add_Ready(Emzi0767.Utilities.AsyncEventHandler (fun client readyEventArgs ->
            client.Logger.LogInformation(botEventId, "Client is ready to process events.")

            Task.CompletedTask
        ))

        client.add_GuildAvailable(Emzi0767.Utilities.AsyncEventHandler (fun client e ->
            Task.CompletedTask
        ))

        client.add_MessageCreated (Emzi0767.Utilities.AsyncEventHandler (fun client e ->
            Ranking.Main.handle e
            cmd client e

            Task.CompletedTask
        ))

        client.add_GuildRoleDeleted (Emzi0767.Utilities.AsyncEventHandler (fun client e ->
            Role.Main.guildRoleDeletedHandler e

            Task.CompletedTask
        ))

        client.add_ClientErrored(Emzi0767.Utilities.AsyncEventHandler (fun client e ->
            client.Logger.LogError(botEventId, e.Exception, "Exception occured", [||])

            Task.CompletedTask
        ))
        client.add_ComponentInteractionCreated (Emzi0767.Utilities.AsyncEventHandler (fun client e ->
            client.Logger.LogInformation(botEventId, "Component created", [||])
            AppsHub.resp client e

            Task.CompletedTask
        ))

        client.add_VoiceStateUpdated (Emzi0767.Utilities.AsyncEventHandler (fun client e ->
            ChatVoice.Main.voiceHandle e
            VoiceChannelNotification.Main.voiceHandle e

            Task.CompletedTask
        ))

        client.add_GuildMemberAdded (Emzi0767.Utilities.AsyncEventHandler (fun client e ->
            Doorkeeper.Main.handle e

            Task.CompletedTask
        ))

        client.add_GuildMemberRemoved (Emzi0767.Utilities.AsyncEventHandler (fun client e ->
            Task.CompletedTask
        ))

        client.ConnectAsync().GetAwaiter().GetResult()

        (Task.Delay -1).GetAwaiter().GetResult()

        0
