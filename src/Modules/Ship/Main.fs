module Ship.Main
open FsharpMyExtension
open FsharpMyExtension.Either
open DSharpPlus

open Types
open Extensions

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
            let nickOrName (user:Entities.DiscordMember) =
                match user.Nickname with
                | null -> user.Username
                | nick -> nick

            if perc < 50 then
                sprintf "Между %s и %s..." (nickOrName who) (nickOrName whom)
            else
                sprintf "Между %s и %s что-то есть!" (nickOrName who) (nickOrName whom)

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
    { Shared.BotModule.empty with
        MessageCreateEventHandleExclude =
            let exec: _ Parser.Parser =
                Parser.start (fun (client: DiscordClient, e: EventArgs.MessageCreateEventArgs) msg ->
                    reduce e client.CurrentUser.Id msg
                )
            Some exec
    }
