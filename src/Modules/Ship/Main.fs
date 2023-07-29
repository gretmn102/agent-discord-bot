module Ship.Main
open FsharpMyExtension
open FsharpMyExtension.Either
open DSharpPlus
open DiscordBotExtensions
open DiscordBotExtensions.Types
open DiscordBotExtensions.Extensions

type ShipOption =
    | Rand
    | Target of int

type Msg =
    | Ship of ShipOption * UserId option
    | MassShip of UserId list

module Parser =
    open FParsec

    open DiscordMessage.Parser

    type 'a Parser = Parser<'a, unit>

    let pship: _ Parser =
        let ptarget =
            pint32
            >>= fun x ->
               if 0 <= x && x <= 100 then
                   preturn x
               else
                   fail "Значение должно быть от 0 до 100 включительно"

        skipString "ship"
        >>? ((ptarget |>> Target) <|> (skipStringCI "rand" >>% Rand))

    let pmassShip =
        skipStringCI "massShip" .>> spaces
        >>. many (puserMention .>> spaces)

    let start f: _ Parser =
        choice [
            pship .>> spaces .>>. opt puserMention |>> Ship
            pmassShip |>> MassShip
        ]
        >>= fun msg ->
            preturn (fun x -> f x msg)

let r = System.Random ()

let binarySearch compare length =
    let rec f from to' =
        let diff = to' - from

        if diff = 0 then
            let res = compare from
            if res = 0 then
                Choice1Of2 from
            elif res > 0 then
                Choice2Of2 (from - 1, from)
            else
                Choice2Of2 (from, from + 1)
        elif diff = 1 then
            let res = compare from
            if res = 0 then
                Choice1Of2 from
            elif res < 0 then
                let from = from + 1
                let res = compare from
                if res = 0 then
                    Choice1Of2 from
                elif res > 0 then
                    Choice2Of2 (from - 1, from)
                else
                    Choice2Of2 (from, from + 1)
            else
                Choice2Of2 (from - 1, from)
        else
            let average = diff / 2
            let idx = from + average
            let res = compare idx
            if res = 0 then
                Choice1Of2 idx
            elif res > 0 then
                f from (idx - 1)
            else
                f (idx + 1) to'

    if length > 0 then
        f 0 (length - 1)
    else
        invalidArg "length" "Length must be greater then 0"

let getDescription =
    let results =
        [|
            0  , "Ничто в мире не даётся без труда — даже любовь..."
            9  , "Ну, ничего, будет и на вашей улице свадьба... Но не сегодня."
            19 , "Любовь зла... как хорошо, что её тут нет!"
            29 , "Сейчас посмотрим Ваш диагноз. Ну, среди всего любви точно нет."
            39 , "Вы испытываете чувства, но не стараетесь! Давай попробуй ещё раз, я в вас верю!"
            49 , "Воздух переполнен страстью, витающей в нём. Задыхаюсь! 😱"
            59 , "Чистые и искренние чувства, которые ещё предстоит проверить на прочность."
            69 , "Кажется, вырисовываются жених и невеста!"
            79 , "Влюбиться — влюбились, осталось взрастить любовь..."
            89 , "Если бы любовь можно было бы измерить деньгами, эти двое были бы уже миллионерами."
            99 , "За такие чувства не жалко и умереть!"
            100, "Дошипперились? Теперь женитесь и живите долго и счастливо"
        |]
        |> Array.sortBy fst

    fun procent ->
        let res =
            results.Length
            |> binarySearch (fun idx ->
                let item, _ = results.[idx]
                item.CompareTo procent
            )
        match res with
        | Choice1Of2 idx ->
            results.[idx]
        | Choice2Of2(lb, ub) ->
            let ub = if ub < results.Length then ub else results.Length - 1
            results.[ub]
        |> snd

let cmdBuilder2
    (e: EventArgs.MessageCreateEventArgs)
    (botId: UserId)
    (opt: ShipOption) usersIds (whomAuthorPhrase: string) (whomBotPhrase: string) =

    let authorId = e.Author.Id

    let send (whoId:UserId) (whomId:UserId) =
        // let who = e.Guild.Members.[whoId] // fired "The given key '...' was not present in the dictionary."
        // let whom = e.Guild.Members.[whomId]
        let who = await (e.Guild.GetMemberAsync whoId)
        let whom = await (e.Guild.GetMemberAsync whomId)

        let perc =
            match opt with
            | Target x -> x
            | Rand -> r.Next(0, 101)

        let embed = Entities.DiscordEmbedBuilder()
        embed.Color <- Entities.Optional.FromValue(DiscordEmbed.backgroundColorDarkTheme)
        embed.Description <-
            getDescription perc

        let imageName = "ship.png"

        embed.WithImageUrl (sprintf "attachment://%s" imageName) |> ignore

        let b = Entities.DiscordMessageBuilder()
        b.Embed <- embed.Build()

        let user1Avatar = WebClientDownloader.getData [] who.AvatarUrl
        let user2Avatar = WebClientDownloader.getData [] whom.AvatarUrl

        use m = new System.IO.MemoryStream()
        Core.img user1Avatar user2Avatar perc m
        m.Position <- 0L
        b.AddFile(imageName, m) |> ignore

        awaiti (e.Channel.SendMessageAsync b)

    match usersIds with
    | [] ->
        match e.Message.ReferencedMessage with
        | null ->
            awaiti (e.Channel.SendMessageAsync "Нужно указать пользователя")
        | referencedMessage ->
            let whomId = referencedMessage.Author.Id
            if whomId = authorId then
                awaiti (e.Channel.SendMessageAsync whomAuthorPhrase)
            elif whomId = botId then
                awaiti (e.Channel.SendMessageAsync whomBotPhrase)
            else
                send e.Author.Id whomId
    | [whomId] ->
        if whomId = authorId then
            awaiti (e.Channel.SendMessageAsync whomAuthorPhrase)
        elif whomId = botId then
            awaiti (e.Channel.SendMessageAsync whomBotPhrase)
        else
            send authorId whomId
    | whoId::whomId::_ ->
        if whomId = whoId then
            awaiti (e.Channel.SendMessageAsync whomAuthorPhrase)
        elif whoId = botId || whomId = botId then
            awaiti (e.Channel.SendMessageAsync whomBotPhrase)
        else
            send whoId whomId

let reduce (e: EventArgs.MessageCreateEventArgs) (botId: UserId) msg =
    match msg with
    | Ship (opt, whomId) ->
        cmdBuilder2
            e
            botId
            opt
            [ match whomId with Some x -> x | None -> () ]
            "Нельзя с самим собой шипериться!"
            "Нельзя со мной шипериться! :scream_cat:"

    | MassShip usersIds ->
        let f (msg: Entities.DiscordMessage) =
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

                let b = Entities.DiscordMessageBuilder()

                let fileName = "massShip.png"

                use m = new System.IO.MemoryStream()
                Core.massShip usersAvatars m
                m.Position <- 0L
                b.AddFile(fileName, m) |> ignore
                let! _ = Async.AwaitTask (msg.ModifyAsync(b))
                ()
            }

        let msg = await (e.Channel.SendMessageAsync "Processing...")
        Async.Start (f msg)

let create () =
    { BotModule.empty with
        MessageCreateEventHandleExclude =
            let exec: _ Parser.Parser =
                Parser.start (fun (client: DiscordClient, e: EventArgs.MessageCreateEventArgs) msg ->
                    reduce e client.CurrentUser.Id msg
                )
            Some exec
    }
