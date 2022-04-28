module UserInfo.Main
open FsharpMyExtension
open DSharpPlus

open Types

type Request =
    | GetAvatar of UserId option
    | GetUserInfo of UserId option
    | GetGuildInfo of GuildId option

module Parser =
    open FParsec

    open DiscordMessage.Parser

    type 'Result Parser = Primitives.Parser<'Result, unit>

    let pgetAvatar: _ Parser =
        skipStringCI "avatar" >>. spaces
        >>. opt (puserMention <|> puint64)

    let pgetUserInfo: _ Parser =
        skipStringCI "user" >>. spaces
        >>. opt (puserMention <|> puint64)

    let pgetGuildInfo: _ Parser =
        skipStringCI "guild" >>. spaces
        >>. opt puint64

    let start: _ Parser =
        choice [
            pgetAvatar |>> GetAvatar
            pgetUserInfo |>> GetUserInfo
            pgetGuildInfo |>> GetGuildInfo
        ]

let exec (client: DiscordClient) (e: EventArgs.MessageCreateEventArgs) (msg: Request) =
    match msg with
    | GetAvatar otherUserIdOrSelf ->
        awaiti <| e.Channel.TriggerTypingAsync()

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

    | GetUserInfo otherUserIdOrSelf ->
        awaiti <| e.Channel.TriggerTypingAsync()

        let message =
            let createMessage (user: Entities.DiscordUser) =

                let embed =
                    let status =
                        match user.Presence with
                        | null -> None
                        | presence ->
                            match presence.Status with
                            | Entities.UserStatus.Online -> "<:online:636551903299371008> Онлайн"
                            | Entities.UserStatus.DoNotDisturb -> "<:dnd:636551902716362764> Не беспокоить"
                            | Entities.UserStatus.Idle -> "<:idle:636551903123210269> Нет на месте"
                            | Entities.UserStatus.Invisible -> "<:offline:636551904096157697> Прячется 🕵️" // not working, unfortunately
                            | Entities.UserStatus.Offline -> "<:offline:636551904096157697> Не в сети"
                            | _ -> "Не определен"
                            |> Some

                    let clientStatus =
                        match user.Presence with
                        | null -> None
                        | presence ->
                            presence.Activities
                            |> Seq.tryPick (fun activity ->
                                if activity.ActivityType = Entities.ActivityType.Custom then
                                    Some <| sprintf "%s %s" (activity.CustomStatus.Emoji.GetDiscordName()) activity.CustomStatus.Name
                                else
                                    None
                            )

                    let mainInfo =
                        match user with
                        | :? Entities.DiscordMember as guildMember ->
                            [
                                sprintf "**Имя:** %s#%s (%d)" guildMember.Username guildMember.Discriminator guildMember.Id

                                match status with
                                | Some status ->
                                    sprintf "**Статус:** %s" status
                                | None -> ()

                                match clientStatus with
                                | Some clientStatus ->
                                    sprintf "**Пользовательский статус:** %s" clientStatus
                                | None -> ()

                                let creationTimestamp = DateTime.Unix.toSec user.CreationTimestamp.UtcDateTime
                                sprintf "**Дата регистрации**: <t:%d:D> (<t:%d:R>)" creationTimestamp creationTimestamp

                                let joinedAt = DateTime.Unix.toSec guildMember.JoinedAt.UtcDateTime
                                sprintf "**Присоединился**: <t:%d:D> (<t:%d:R>)" joinedAt joinedAt
                            ] |> String.concat "\n"

                        | user ->
                            [
                                sprintf "**Имя:** %s#%s (%d)" user.Username user.Discriminator user.Id

                                match status with
                                | Some status ->
                                    sprintf "**Статус:** %s" status
                                | None -> ()

                                match clientStatus with
                                | Some clientStatus ->
                                    sprintf "**Пользовательский статус:** %s" clientStatus
                                | None -> ()

                                let creationTimestamp = DateTime.Unix.toSec user.CreationTimestamp.UtcDateTime
                                sprintf "**Дата регистрации**: <t:%d:D> (<t:%d:R>)" creationTimestamp creationTimestamp
                            ] |> String.concat "\n"

                    Entities.DiscordEmbedBuilder()
                        .WithAuthor(user.Username, iconUrl = user.AvatarUrl)
                        .WithColor(if user.BannerColor.HasValue then user.BannerColor.Value else Entities.DiscordColor "#2f3136" )
                        .AddField("Основная информация", mainInfo, true)
                        .WithThumbnail(user.AvatarUrl)
                        .Build()

                let b = Entities.DiscordMessageBuilder()
                b.Embed <- embed
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
                | user -> createMessage user
            | None -> createMessage e.Author

        ignore (await (e.Channel.SendMessageAsync message))

    | GetGuildInfo otherGuildIdOrSelf ->
        awaiti <| e.Channel.TriggerTypingAsync()

        let message =
            let createMessage (guild: Entities.DiscordGuild) =
                let embed =
                    let fields =
                        [
                            let info =
                                guild.Members
                                |> Seq.fold
                                    (fun (info: {| membersCount: int; botsCount: int; statuses: Map<_, _> |}) (KeyValue(_, user)) ->
                                        let info =
                                            if user.IsBot then
                                                {| info with
                                                    botsCount = info.botsCount + 1 |}
                                            else
                                                {| info with
                                                    membersCount = info.membersCount + 1
                                                |}

                                        {| info with
                                            statuses =
                                                match user.Presence with
                                                | null ->
                                                    info.statuses
                                                    |> Map.addOrModWith
                                                        Entities.UserStatus.Offline
                                                        (fun () -> 1)
                                                        (fun acc -> acc + 1)

                                                | presence ->
                                                    info.statuses
                                                    |> Map.addOrModWith
                                                        presence.Status
                                                        (fun () -> 1)
                                                        (fun acc -> acc + 1)
                                        |}
                                    )
                                    {| membersCount = 0; botsCount = 0; statuses = Map.empty |}

                            "Участники:", [
                                sprintf "Всего: **%d**" guild.MemberCount
                                sprintf "Людей: **%d**" info.membersCount
                                sprintf "Ботов: **%d**" info.botsCount
                            ] |> String.concat "\n"

                            let statuses =
                                let f status = Map.tryFind status info.statuses |> Option.defaultValue 0
                                [
                                    sprintf "<:online:636551903299371008> В сети: **%d**" (f Entities.UserStatus.Online)
                                    sprintf "<:idle:636551903123210269> Нет на месте: **%d**" (f Entities.UserStatus.Idle)
                                    sprintf "<:dnd:636551902716362764> Не беспокоить: **%d**" (f Entities.UserStatus.DoNotDisturb)
                                    sprintf "<:offline:636551904096157697> Не в сети: **%d**" (f Entities.UserStatus.Offline)
                                ] |> String.concat "\n"
                            "По статусам:", statuses

                            "Владелец:", sprintf "<@!%d>" guild.OwnerId

                            let dateTime = DateTime.Unix.toSec guild.CreationTimestamp.UtcDateTime
                            "Дата создания:", sprintf "<t:%d:D> (<t:%d:R>)" dateTime dateTime
                        ]

                    let embed =
                        Entities.DiscordEmbedBuilder()
                            .WithColor(Entities.DiscordColor "#2f3136")
                            .WithThumbnail(guild.IconUrl)
                            .WithImageUrl(guild.BannerUrl)

                    let embed =
                        fields
                        |> List.fold
                            (fun (b: Entities.DiscordEmbedBuilder) (header, value) ->
                                b.AddField(header, value, true)
                            )
                            embed

                    embed.Build()

                let b = Entities.DiscordMessageBuilder()
                b.Embed <- embed
                b

            match otherGuildIdOrSelf with
            | Some guildId ->
                let guild =
                    try
                        await (client.GetGuildAsync guildId)
                    with e -> null

                match guild with
                | null ->
                    let b = Entities.DiscordMessageBuilder()
                    b.Content <- sprintf "Guild %d not found" guildId
                    b
                | guild -> createMessage guild
            | None -> createMessage e.Guild

        ignore (await (e.Channel.SendMessageAsync message))
