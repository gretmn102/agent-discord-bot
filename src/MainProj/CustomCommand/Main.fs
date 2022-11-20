module CustomCommand.Main
open FsharpMyExtension
open FsharpMyExtension.Either
open DSharpPlus

open Types
open Extensions

[<Struct>]
type Act =
    | Take
    | Fairytail
    | Catail
    | Bully
    | Admire
    | Battery
    | Catch
    | Angry
    | Kebab
    | FortuneCookies
    | Bite

type Msg = Act * UserId option

module Parser =
    open FParsec

    open DiscordMessage.Parser

    type 'a Parser = Parser<'a, unit>

    let cmd: _ Parser =
        choice [
            skipStringCI "take" >>% Take
            skipStringCI "fairytail" >>% Fairytail
            skipStringCI "catail" >>% Catail
            skipStringCI "bully" >>. optional (skipStringCI "ing") >>% Bully
            skipStringCI "admire" >>% Admire
            skipStringCI "battery" >>% Battery
            skipStringCI "catch" >>% Catch
            skipStringCI "angry" >>% Angry
            skipStringCI "печенье" >>% FortuneCookies
            (skipStringCI "шашлычок" <|> skipStringCI "шашлык") >>% Kebab
            skipStringCI "кусь" >>% Bite
        ]

    let start f: _ Parser =
        cmd .>> spaces .>>. opt puserMention
        >>= fun msg ->
            preturn (fun x -> f x msg)

let r = System.Random ()

let cmdBuilder
    (client: DiscordClient)
    (e: EventArgs.MessageCreateEventArgs)
    whomId
    (gifs: string [])
    content
    (whomAuthorPhrase:string)
    (whomBotPhrase:string) =

    let send whomId =
        let whom =
            await <| client.GetUserAsync whomId

        let b = Entities.DiscordEmbedBuilder()
        b.Description <- content e.Author.Username whom.Username

        b.Color <- Entities.Optional.FromValue(DiscordEmbed.backgroundColorDarkTheme)
        if not (Array.isEmpty gifs) then
            b.ImageUrl <- gifs.[r.Next(0, gifs.Length)]

        awaiti (client.SendMessageAsync (e.Channel, b.Build()))

    let authorId = e.Author.Id
    let botId = client.CurrentUser.Id

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

let takeUrls =
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

let kebabs =
    [|
        "https://media.tenor.com/LtTjLfArzqAAAAAd/sheftalia-karvouna.gif"
        "https://media.tenor.com/Xufgg6gSRPkAAAAC/%E1%83%9B%E1%83%AC%E1%83%95%E1%83%90%E1%83%93%E1%83%98-mwvadi.gif"
        "https://media.tenor.com/ayzVsly9VGEAAAAd/%E1%83%90%E1%83%97%E1%83%9A%E1%83%94%E1%83%97%E1%83%98%E1%83%99%E1%83%90%E1%83%AA%E1%83%98-%E1%83%9B%E1%83%94%E1%83%A5%E1%83%90%E1%83%91%E1%83%90%E1%83%91%E1%83%94%E1%83%AF%E1%83%94%E1%83%9A%E1%83%90%E1%83%9A%E1%83%98.gif"
        "https://media.tenor.com/_ZQHECcQ1nsAAAAd/%D1%88%D0%B0%D1%88%D0%BB%D1%8B%D0%BA-%D0%B1%D0%B5%D0%B1%D1%80%D0%B0.gif"
        "https://media.tenor.com/qeU8IezNdJIAAAAS/%EC%8B%9D%ED%92%88%EA%B0%80%EA%B3%B5.gif"
        "https://media.tenor.com/1jIM6pxDDdMAAAAC/bbq-meat.gif"
        "https://media.tenor.com/RdEYFJcw8BYAAAAC/brasero.gif"
    |]

let reduce (client: DiscordClient) (e: EventArgs.MessageCreateEventArgs) ((msg, whomId): Msg) =
    let cmdBuilder = cmdBuilder client e

    match msg with
    | Take ->
        cmdBuilder
            whomId
            takeUrls
            (sprintf "**%s** носит на ручках **%s**")
            "Самого себя нельзя на руках носить :eyes:"
            "Меня не нужно носить! :scream_cat:"
    | Fairytail ->
        cmdBuilder
            whomId
            fairytailGifs
            (sprintf "**%s** читает сказку **%s**")
            "Нельзя себе сказку читать :eyes:"
            "Мне не нужно сказки читать! :scream_cat:"
    | Catail ->
        cmdBuilder
            whomId
            catailGifs
            (fun who whom -> sprintf "**%s**, **%s** машет тебе хвостом" whom who)
            "Нельзя себе хвостом махать, хотя..."
            "Мне не нужно хвостом махать! :scream_cat:"
    | Bully ->
        cmdBuilder
            whomId
            [||]
            (sprintf "**%s** буллит **%s** <:Demon_Kingsmile:877678191693692969>")
            "Себя нельзя буллить! Хотя..."
            "Мне нельзя буллить! :scream_cat:"
    | Admire ->
        cmdBuilder
            whomId
            admireGifs
            (sprintf "**%s** любуется **%s**")
            "Нельзя любоваться собой :eyes:"
            "Нельзя мною любоваться :scream_cat:"
    | Battery ->
        cmdBuilder
            whomId
            batteryGifs
            (sprintf "**%s** поет \"Батарейку\" **%s**")
            "Самому нельзя петь \"Батерей\"!"
            "Мне нельзя петь \"Батарейку\". Я этого не вынесу :scream_cat:"
    | Catch ->
        cmdBuilder
            whomId
            catchAssets
            (sprintf "**%s** ловит **%s**")
            "Самого нельзя ловить!"
            "Меня нельзя ловить! Я этого не вынесу :scream_cat:"
    | Angry ->
        cmdBuilder
            whomId
            angryAssets
            (sprintf "**%s** злится на **%s**")
            "На самого себя нельзя злиться, ну в самом деле!"
            "На меня не надо злиться, я хороший!"
    | Bite ->
        let assets =
            [|
                "https://media.tenor.com/iFjm7dyo_-MAAAAd/cat-bite.gif"
                "https://media.tenor.com/WVRAumVRwi4AAAAS/cat.gif"
                "https://media.tenor.com/wI_QPAY2G1cAAAAd/cat-bites-lip.gif"
                "https://media.tenor.com/yVKQAhFuGZQAAAAC/cat-bite.gif"
                "https://media.tenor.com/bB9rBu4CZxoAAAAd/cat-bite.gif"
                "https://media0.giphy.com/media/XtuYDes6uyL4Y/giphy.gif"
            |]

        cmdBuilder
            whomId
            assets
            (sprintf "**%s** куськает **%s**")
            "Не надо самого куськать, ну в самом деле!"
            "Меня нельзя куськать: я железный и невкусный! 🙀"

    | Kebab ->
        let predictions = kebabs

        let send whomId =
            let whom =
                whomId
                |> Option.map (fun whomId ->
                    try
                        await (client.GetUserAsync whomId)
                        |> Some
                        |> Ok
                    with e ->
                        sprintf "Пользователя %d не существует" whomId
                        |> Error
                )
                |> Option.defaultValue (Ok None)

            let mkContent () =
                match whom with
                | Ok whom ->
                    match whom with
                    | None ->
                        sprintf "<@%d> жарит себе шашлычок."
                            e.Author.Id
                    | Some whom ->
                        let authorId = e.Author.Id
                        let botId = client.CurrentUser.Id

                        if whom.Id = authorId then
                            sprintf "<@%d> жарит себе шашлычок."
                                authorId
                        elif whom.Id = botId then
                            sprintf "<@%d> жарит мне шашлычок, пасеба 😋"
                                authorId
                        else
                            sprintf "<@%d> делает шашлычок для <@%d>."
                                authorId
                                whom.Id
                | Error errMsg -> errMsg

            let prediction =
                predictions.[r.Next(0, predictions.Length)]

            let b = Entities.DiscordEmbedBuilder()
            b.Description <- mkContent ()
            b.Color <- Entities.Optional.FromValue(DiscordEmbed.backgroundColorDarkTheme)
            b.ImageUrl <- prediction

            awaiti (client.SendMessageAsync (e.Channel, b.Build()))

        match whomId with
        | Some whomId ->
            send (Some whomId)
        | None ->
            match e.Message.ReferencedMessage with
            | null ->
                send whomId
            | referencedMessage ->
                send (Some referencedMessage.Author.Id)

    | FortuneCookies ->
        let predictions =
            [|
                "Всё будет хорошо <:Demon_Kingsmile:877678191693692969>"
                "Некто из прошлого встретится с тобой в этом месяце 🖖"
                "Сегодня свободное время лучше всего уделить учёбе ✍️"
                "Не пей на ночь много жидкости, даже если это ром, арр! 🏴‍☠️"
                "Сегодня лучший день, чтобы отведать новое блюдо!"
                "Прогулка по палубе и хорошая пиратская музыка — вот что приведёт твои мысли в порядок, арр! 🏴‍☠️"
                "Котан, сегодня особый риск — береги хвост!"
                "После сильного шторма обязательно наступает безветрие — будь силён и терпелив."
                "Какую бы русалку или сирену ты не встретил, не позволяй ей собой командовать! 🧜‍♀️"
                "Богиня печенек не одобрит того, что ты делаешь с печеньем <:satana:901368411295215636>"
                "Берегись чаек: эти ворюги утащат весь улов 👻"
                "Держись за мачту во время шторма! 🌪️"
                "С крякеном можно дружить, даже если он размером с корабль и готов потопить всё на свете! <:satana:901368411295215636>"
                "Бойся дождя из печенек! <:satana:901368411295215636>"

                [
                    "Бывает Лу́ня милой,"
                    "Бывает Луня злой,"
                    "Бывает нетерпимой,"
                    "Беги, пока живой <:Demon_Kingsmile:877678191693692969>"
                ] |> String.concat "\n"

                "Не суди о сюрстрёмминге по одному лишь запаху."
                "Иногда лучшее решение — плыть по течению."
                "После всего пережитого тебе ещё не хочется надеть на глаз повязку, завести говорящего попугая и бахнуть рома?"
                "Безветренная погода — время для упорной работы, а только после неё — время для отдыха."
                "Попробуй связать тельняшку, пока не замёрз. Связал себе — свяжи для друга."
                "Не бойся гавани, где однажды потерпел крушение."
                "Учись вязать узлы — пригодится!"
                "Прилив сил прямо по курсу, не пропусти!"
                "Не учи нырять утку, а рыбу — плавать!"
                "Сделай глубокий вдох и — вперёд! Достижение желаемой цели уже близко."
                "Сокровища не заставят себя ждать на этой неделе."
                "Будь на чеку, моряк, скоро тебя испытает шторм!"
                "Жди поцелуя. Что это будет: романтическое приключение или столкновение кораблей носами?"
                "Корабль без простора морского, что сердце без любви. Оглянись вокруг и насладись простором."
                "Судно — дом моряка, так наведи же дома порядок!"
                "Тебе стоит поговорить с кем-нибудь на корабле."
                "Сегодня кому-то придётся мыть гальюн — молись, чтобы не пришлось мыть тебе! 👻"
                "Брось за борт неприятные мысли."
                "Сегодня нужно лечь спать пораньше, чтобы завтра с новыми силами покорять моря!"
                "Кому-то из твоих друзей необходимы объятия. Обними его."
                "Будешь налегать на печеньки с предсказаниями — растолстеешь! Тогда команда корабля будет использовать тебя в качестве якоря."
                "Сегодня чудная погода за бортом — самое время взять мольберт с красками и нарисовать это великолепие! Не забудь выложить свой шедевр в <#876547999675871232>."
                "Грядёт счастливая волна — поймаешь её на сёрфе или останешься сидеть в трюме?"
                "Проверь, что карта, компас и подзорная труба лежат на месте, а то мало ли, вдруг потерялись."
                "Взгляни на своё отражение на водной глади, и кто знает, быть может, увидишь там самое чудесное создание на свете 👻"
            |]

        let send whomId =
            let whom =
                whomId
                |> Option.map (fun whomId ->
                    try
                        await (client.GetUserAsync whomId)
                        |> Some
                        |> Ok
                    with e ->
                        sprintf "Пользователя %d не существует" whomId
                        |> Error
                )
                |> Option.defaultValue (Ok None)

            let mkContent prediction =
                match whom with
                | Ok whom ->
                    match whom with
                    | None ->
                        sprintf "<@%d>, печенька с предсказанием гласит:\n\n%s"
                            e.Author.Id
                            prediction
                    | Some whom ->
                        let authorId = e.Author.Id
                        let botId = client.CurrentUser.Id

                        if whom.Id = authorId then
                            sprintf "<@%d>, печенька с предсказанием гласит:\n\n%s"
                                authorId
                                prediction
                        elif whom.Id = botId then
                            sprintf "<@%d>, мне предсказания не нужны: я и так знаю, что кожанные мешки проиграют машинам 🤖"
                                authorId
                        else
                            sprintf "<@%d> зачитывает печеньку с предсказанием <@%d>:\n\n%s"
                                authorId
                                whom.Id
                                prediction
                | Error errMsg -> errMsg

            let prediction =
                predictions.[r.Next(0, predictions.Length)]

            let b = Entities.DiscordEmbedBuilder()
            b.Description <- mkContent prediction
            b.Color <- Entities.Optional.FromValue(DiscordEmbed.backgroundColorDarkTheme)
            b.ImageUrl <- "https://cdn.discordapp.com/attachments/912291464074117161/1034055256432193637/l-intro-1608226504-removebg-preview.png"

            awaiti (client.SendMessageAsync (e.Channel, b.Build()))

        match whomId with
        | Some whomId ->
            send (Some whomId)
        | None ->
            match e.Message.ReferencedMessage with
            | null ->
                send whomId
            | referencedMessage ->
                send (Some referencedMessage.Author.Id)

let exec: MessageCreateEventHandler Parser.Parser =
    Parser.start (fun (client: DiscordClient, e: EventArgs.MessageCreateEventArgs) msg ->
        reduce client e msg
    )
