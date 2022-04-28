module CustomCommand.Main
open FsharpMyExtension
open FsharpMyExtension.Either
open DSharpPlus

open Types

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
        ]

    let start: Msg Parser =
        cmd .>> spaces .>>. opt puserMention

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

        b.Color <- Entities.Optional.FromValue(Entities.DiscordColor("#2f3136"))
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

let exec (client: DiscordClient) (e: EventArgs.MessageCreateEventArgs) ((msg, whomId): Msg) =
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
