module App
open FsharpMyExtension
open FsharpMyExtension.Either
open Microsoft.Extensions.Logging

open System.Threading.Tasks

let await (t:Task<_>) =
    t.GetAwaiter() |> fun x -> x.GetResult()
let awaiti (t:Task<_>) =
    await t |> ignore

let botEventId = new EventId(42, "Bot-Event")

type UserId = uint64

module Parser =
    open FParsec

    type 'a Parser = Parser<'a, unit>
    let puserMention : _ Parser =
        skipString "<@" >>. optional (skipChar '!') >>. puint64 .>> skipChar '>'
    let puserMentionTarget (userId:UserId) : _ Parser =
        skipString "<@" >>. optional (skipChar '!') >>. skipString (string userId) >>. skipChar '>'

    type Cmd =
        | Take of UserId option
        | Unknown
        | Pass

    let prefix = pchar '.'

    let pcommand =
        pstring "take" >>. spaces >>. opt puserMention |>> Take

    let start botId str =
        let p =
            (puserMentionTarget botId >>. spaces >>. (optional prefix >>. pcommand <|>% Unknown))
            <|> (prefix >>. pcommand <|>% Pass)
        match run p str with
        | Success(x, _, _) -> Right x
        | Failure(x, _, _) -> Left x

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

let cmd (client:DSharpPlus.DiscordClient) (e:DSharpPlus.EventArgs.MessageCreateEventArgs) =
    let authorId = e.Author.Id
    let botId = client.CurrentUser.Id

    let send whomId =
        let whom =
            client.GetUserAsync whomId
            |> fun x -> x.GetAwaiter().GetResult()
        let content =
            sprintf "**%s** носит на ручках **%s**" e.Author.Username whom.Username

        let b = DSharpPlus.Entities.DiscordEmbedBuilder()
        b.ImageUrl <- gifUrls.[r.Next(0, gifUrls.Length)]
        b.Description <- content

        awaiti (client.SendMessageAsync (e.Channel, b.Build()))

    if authorId <> botId then
        match Parser.start botId e.Message.Content with
        | Right res ->
            match res with
            | Parser.Pass -> ()
            | Parser.Take whomId ->
                match whomId with
                | Some whomId ->
                    if whomId = authorId then
                        awaiti (client.SendMessageAsync (e.Channel, "Самого себя нельзя на руках носить :eyes:"))
                    elif whomId = botId then
                        awaiti (client.SendMessageAsync (e.Channel, "Меня не нужно носить! :scream_cat: "))
                    else
                        send whomId
                | None ->
                    match e.Message.ReferencedMessage with
                    | null ->
                        awaiti (client.SendMessageAsync (e.Channel, "Нужно указать пользователя"))
                    | referencedMessage ->
                        send referencedMessage.Author.Id
            | Parser.Unknown ->
                awaiti (client.SendMessageAsync (e.Channel, "Неизвестная команда"))
        | Left x ->
            awaiti (client.SendMessageAsync (e.Channel, (sprintf "Ошибка:\n```\n%s\n```" x)))

[<EntryPoint>]
let main argv =
    match System.Environment.GetEnvironmentVariable "DiscordCommandBotToken" with
    | null -> printfn "DiscordCommandBotToken not setup"
    | token ->
        let config = DSharpPlus.DiscordConfiguration()

        config.set_Token token
        config.set_TokenType DSharpPlus.TokenType.Bot
        config.set_AutoReconnect true

        let client = new DSharpPlus.DiscordClient(config)

        client.add_Ready(Emzi0767.Utilities.AsyncEventHandler (fun client readyEventArgs ->
            client.Logger.LogInformation(botEventId, "Client is ready to process events.")

            Task.CompletedTask
        ))

        client.add_GuildAvailable(Emzi0767.Utilities.AsyncEventHandler (fun client e ->
            Task.CompletedTask
        ))

        client.add_MessageCreated (Emzi0767.Utilities.AsyncEventHandler (fun client e ->
            cmd client e

            Task.CompletedTask
        ))

        client.add_ClientErrored(Emzi0767.Utilities.AsyncEventHandler (fun client e ->
            client.Logger.LogError(botEventId, e.Exception, "Exception occured", [||])

            Task.CompletedTask
        ))

        client.ConnectAsync().GetAwaiter().GetResult()

        (Task.Delay -1).GetAwaiter().GetResult()

    0
