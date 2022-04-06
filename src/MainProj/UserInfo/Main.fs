module UserInfo.Main
open DSharpPlus

open Types

type Request =
    | GetAvatar of UserId option

module Parser =
    open FParsec

    open DiscordMessage.Parser

    type 'Result Parser = Primitives.Parser<'Result, unit>

    let pgetAvatar: _ Parser =
        skipStringCI "avatar" >>. spaces
        >>. opt (puserMention <|> puint64)

    let start: _ Parser =
        pgetAvatar |>> GetAvatar

let exec (client: DiscordClient) (e: EventArgs.MessageCreateEventArgs) (msg: Request) =
    match msg with
    | GetAvatar otherUserIdOrSelf ->
        e.Channel.TriggerTypingAsync().GetAwaiter().GetResult()

        let message =
            let createAvatarMessage (user: Entities.DiscordUser) =
                let embed = Entities.DiscordEmbedBuilder()
                embed.WithAuthor(user.Username) |> ignore
                embed.ImageUrl <- user.AvatarUrl
                embed.Color <- Entities.Optional.FromValue(Entities.DiscordColor("#2f3136"))
                let b = Entities.DiscordMessageBuilder()
                b.Embed <- embed.Build()
                b

            match otherUserIdOrSelf with
            | Some userId ->
                let user =
                    try
                        await (client.GetUserAsync userId)
                    with e -> null

                match user with
                | null ->
                    let b = Entities.DiscordMessageBuilder()
                    b.Content <- sprintf "User %d not found" userId
                    b
                | user -> createAvatarMessage user
            | None -> createAvatarMessage e.Author

        ignore (await (e.Channel.SendMessageAsync message))
