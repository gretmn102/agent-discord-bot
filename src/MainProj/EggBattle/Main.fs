module EggBattle.Main
open FsharpMyExtension
open FsharpMyExtension.Either
open DSharpPlus

open Types
open Model

type Request =
    | ChallengeToDuel of UserId
    | CreateRatingTable

module Parser =
    open FParsec

    open DiscordMessage.Parser

    type 'Result Parser = Primitives.Parser<'Result, unit>

    let challangeToDuelName = "битва"

    let pchannelToDuel: _ Parser =
        skipStringCI challangeToDuelName >>. spaces
        >>. (puserMention <|> puint64)

    let start: _ Parser =
        choice [
            pchannelToDuel |>> ChallengeToDuel
            skipStringCI "лидеры" >>% CreateRatingTable
        ]

type State =
    {
        Rating: Rating.GuildsRating
    }

type FightState =
    {
        AttackerId: UserId
        DefenderId: UserId
    }

[<Struct>]
type ResultBattle =
    | AttackerWin
    | DefenderWin

type Req =
    | Request of EventArgs.MessageCreateEventArgs * Request
    | Fight of FightState * EventArgs.ComponentInteractionCreateEventArgs
    | GetGuildRating of GuildId * AsyncReplyChannel<Map<UserId,Rating.Data>>

module RatingTable =
    [<Struct>]
    type SortBy = SortByWins

    [<Literal>]
    let RefreshButtonId = "EggBattleRefreshButtonId"
    [<Literal>]
    let LeftArrowButtonId = "EggBattleLeftArrowButtonId"
    [<Literal>]
    let PaginationButtonId = "EggBattlePaginationButtonId"
    [<Literal>]
    let RightArrowButtonId = "EggBattleRightArrowButtonId"

    let createTable
        (guild: Entities.DiscordGuild)
        (addComponents: Entities.DiscordComponent [] -> _)
        addEmbed
        page
        (state: Map<UserId,Rating.Data>) =

        let itemsCountPerPage = 10
        let sortBy = SortByWins

        let itemsCount = state.Count
        let lastPageItemsCount = itemsCount % itemsCountPerPage
        let pagesCount = itemsCount / itemsCountPerPage + if lastPageItemsCount > 0 then 1 else 0

        let table =
            let sortFunction: Map<UserId,Rating.Data> -> _ =
                match sortBy with
                | SortByWins -> Seq.sortByDescending (fun x -> x.Value.Wins)

            let headers = [| "Игрок"; "Победы▼"; "Поражения" |]

            let table =
                if 0 < page && page <= pagesCount then
                    let lb = (page - 1) * itemsCountPerPage
                    let ub =
                        if page = pagesCount then if lastPageItemsCount > 0 then lastPageItemsCount else itemsCountPerPage
                        else itemsCountPerPage

                    state
                    |> sortFunction
                    |> Seq.skip lb |> Seq.take ub
                    |> Seq.mapi (fun i (KeyValue(userId, data)) ->
                        [|
                            sprintf "%d <@!%d>" (lb + i + 1) userId
                            string data.Wins
                            string data.Loses
                        |]
                    )
                    |> Array.ofSeq
                    |> Array.transpose
                    |> Array.map (String.concat "\n")

                else
                    Array.replicate headers.Length ""

            table
            |> Array.map2 (fun header value -> header, value) headers

        let embed =
            let b =
                Entities.DiscordEmbedBuilder()
                    .WithColor(Entities.DiscordColor "#2f3136")
                    .WithTitle("Битва на яйцах!")

            let b =
                table
                |> Array.fold
                    (fun (b: Entities.DiscordEmbedBuilder) (header, value) ->
                        // System.ArgumentException: Value cannot be empty or whitespace. (Parameter 'value')
                        let value = if value = "" then "x" else value

                        b.AddField(header, value, true)
                    )
                    b

            b.Build()

        addComponents [|
            Entities.DiscordButtonComponent(
                ButtonStyle.Secondary,
                RefreshButtonId,
                "",
                emoji = Entities.DiscordComponentEmoji(Name = "🔄") // :arrows_counterclockwise:
            )

            Entities.DiscordButtonComponent(
                ButtonStyle.Secondary,
                LeftArrowButtonId,
                "",
                disabled = (page <= 1),
                emoji = Entities.DiscordComponentEmoji(Name = "⬅️")
            )
            Entities.DiscordButtonComponent(
                ButtonStyle.Secondary,
                PaginationButtonId,
                sprintf "%d/%d" page pagesCount,
                disabled = true
            )
            Entities.DiscordButtonComponent(
                ButtonStyle.Secondary,
                RightArrowButtonId,
                "",
                disabled = (page >= pagesCount),
                emoji = Entities.DiscordComponentEmoji(Name = "➡️")
            )
        |]
        |> ignore

        addEmbed embed |> ignore

    let componentInteractionCreateHandle getState (client: DiscordClient) (e: EventArgs.ComponentInteractionCreateEventArgs) =
        let getCurrentPage () =
            e.Message.Components
            |> Seq.tryPick (fun row ->
                row.Components
                |> Seq.tryPick (fun row ->
                    if row.CustomId = PaginationButtonId then
                        let paginationButton = row :?> Entities.DiscordButtonComponent
                        let page =
                            let label = paginationButton.Label
                            let slashIndex = label.IndexOf "/"
                            int label.[..slashIndex - 1]
                        Some page
                    else
                        None
                )
            )

        let update getPage =
            let currentPage =
                match getCurrentPage () with
                | Some currentPage -> currentPage

                | None -> 1

            let state = getState ()

            let b = Entities.DiscordInteractionResponseBuilder()

            createTable e.Guild b.AddComponents b.AddEmbed (getPage currentPage) state

            e.Interaction.CreateResponseAsync(InteractionResponseType.UpdateMessage, b).GetAwaiter().GetResult()


        if e.Message.Author.Id = client.CurrentUser.Id then
            match e.Id with
            | RefreshButtonId ->
                update id
                true
            | LeftArrowButtonId ->
                update (fun currentPage -> currentPage - 1)
                true
            | RightArrowButtonId ->
                update (fun currentPage -> currentPage + 1)
                true
            | _ -> false

        else
            false

[<Literal>]
let YesButtonId = "EggBattleYesButtonId"

[<Literal>]
let NoButtonId = "EggBattleNoButtonId"

let r = new System.Random()

let reduce msg (state: State) =
    match msg with
    | Request(e, r) ->
        match r with
        | ChallengeToDuel userId ->
            e.Channel.TriggerTypingAsync().GetAwaiter().GetResult()

            if e.Author.Id = userId then
                let b =
                    Entities.DiscordMessageBuilder()
                        .WithContent(sprintf "<@!%d>, нельзя самого себя вызывать на бой!" e.Author.Id)

                awaiti <| e.Channel.SendMessageAsync(b)
            else
                let targetUser =
                    try
                        await <| e.Guild.GetMemberAsync userId
                        |> Right
                    with _ ->
                        sprintf "Пользователь с %d ID не найден" userId
                        |> Left

                match targetUser with
                | Right _ ->
                    let b = Entities.DiscordMessageBuilder()

                    b.Embed <-
                        Entities.DiscordEmbedBuilder(
                            Color = Entities.Optional.FromValue(Entities.DiscordColor "#2f3136"),
                            Title = "Вызов на дуэль!",
                            Description = sprintf "<@!%d>, <@!%d> вызывает тебя на бой на 🥚! Соглашаешься?! <:angry:927633404353196113>" userId e.Author.Id
                        ).Build()

                    let applyToPartialState str =
                        let partialStateStr =
                            { AttackerId = e.Author.Id; DefenderId = userId }
                            |> Json.serNotIndent
                        str + partialStateStr

                    let buttons: Entities.DiscordComponent [] =
                        [|
                            Entities.DiscordButtonComponent(
                                ButtonStyle.Primary,
                                applyToPartialState YesButtonId,
                                "Да"
                            )
                            Entities.DiscordButtonComponent(
                                ButtonStyle.Danger,
                                applyToPartialState NoButtonId,
                                "Нет"
                            )
                        |]

                    buttons
                    |> b.AddComponents
                    |> ignore

                    awaiti <| e.Channel.SendMessageAsync b
                | Left msgErr ->
                    awaiti <| e.Channel.SendMessageAsync msgErr

            state
        | CreateRatingTable ->
            e.Channel.TriggerTypingAsync().GetAwaiter().GetResult()

            let b = Entities.DiscordMessageBuilder()

            match Map.tryFind e.Guild.Id state.Rating with
            | Some guildRating ->
                RatingTable.createTable e.Guild b.AddComponents b.AddEmbed 1 guildRating
            | None -> b.Content <- "Таблица пока что пуста."

            awaiti <| e.Channel.SendMessageAsync b

            state

    | Fight(fightState, e) ->
        let winnerId, loserId =
            match r.Next(0, 2) with
            | 0 -> fightState.AttackerId, fightState.DefenderId
            | 1 -> fightState.DefenderId, fightState.AttackerId
            | i -> failwithf "Internal error: expected 0, 1 but %d" i

        let ratingState =
            let guildId = e.Guild.Id

            let apply guildRating =
                guildRating
                |> Map.addOrModWith
                    winnerId
                    (fun () ->
                        Rating.insert(guildId, winnerId, 1, 0)
                    )
                    (fun (attacker: Rating.Data) ->
                        let attacker =
                            { attacker with
                                Wins = attacker.Wins + 1
                            }
                        Rating.replace attacker
                        attacker
                    )
                |> Map.addOrModWith
                    loserId
                    (fun () ->
                        Rating.insert(guildId, loserId, 0, 1)
                    )
                    (fun (loser: Rating.Data) ->
                        let loser =
                            { loser with
                                Loses = loser.Loses + 1
                            }
                        Rating.replace loser
                        loser
                    )

            state.Rating
            |> Map.addOrModWith
                guildId
                (fun () -> apply Map.empty)
                (fun rating -> apply rating)

        let b = Entities.DiscordInteractionResponseBuilder()

        let embed =
            Entities.DiscordEmbedBuilder()
                .WithColor(Entities.DiscordColor "#2f3136")
                .WithTitle("Итог сражения!")
                .WithDescription(sprintf "И в схватке на 🥚 между <@!%d> и <@!%d> побеждает... <@!%d>! 🎉" fightState.AttackerId fightState.DefenderId winnerId)
                .Build()

        b.AddEmbed embed |> ignore

        e.Interaction.CreateResponseAsync(InteractionResponseType.UpdateMessage, b).GetAwaiter().GetResult()

        { state with
            Rating = ratingState
        }

    | GetGuildRating(guildId, r) ->
        Map.tryFind guildId state.Rating
        |> Option.defaultValue Map.empty
        |> r.Reply

        state

let m =
    let init = {
        Rating = Rating.getAll ()
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

let handle e msg =
    m.Post (Request (e, msg))

let (|StartsWith|_|) (value: string) (str: string) =
    if str.StartsWith value then
        Some (str.[value.Length..str.Length - 1])
    else
        None

let componentInteractionCreateHandle (client: DiscordClient) (e: EventArgs.ComponentInteractionCreateEventArgs) =
    if e.Message.Author.Id = client.CurrentUser.Id then
        let f fightStateStr next =
            let fightState =
                try
                    Right (Json.des fightStateStr: FightState)
                with e ->
                    e.Message
                    |> Left

            match fightState with
            | Right fightState ->
                if fightState.DefenderId = e.User.Id then
                    next fightState
                else
                    let b =
                        Entities.DiscordInteractionResponseBuilder()
                            .AsEphemeral(true)
                            .WithContent(sprintf "На эту кнопку должен нажать <@!%d>!" fightState.DefenderId)

                    e.Interaction.CreateResponseAsync(InteractionResponseType.ChannelMessageWithSource, b).GetAwaiter().GetResult()

            | Left errMsg ->
                // remove components
                let msg = e.Message
                let b = Entities.DiscordInteractionResponseBuilder()
                // necessary because throw `System.ArgumentException: You must specify content, an embed, a sticker, or at least one file.`
                b.AddEmbeds msg.Embeds |> ignore
                b.Content <- msg.Content
                e.Interaction.CreateResponseAsync(InteractionResponseType.UpdateMessage, b).GetAwaiter().GetResult()

                Entities.DiscordFollowupMessageBuilder()
                    .AsEphemeral(true)
                    .WithContent(
                        [
                            sprintf "Ой, что-то пошло не так. Вызовите снова пользователя на бой командой `.%s @пользователь`:" Parser.challangeToDuelName
                            "```"
                            errMsg
                            "```"
                        ] |> String.concat "\n"
                    )
                |> e.Interaction.CreateFollowupMessageAsync
                |> awaiti

        match e.Id with
        | StartsWith YesButtonId rest ->
            f rest (fun fightState ->
                m.Post (Fight (fightState, e))
            )

            true
        | StartsWith NoButtonId rest ->
            f rest (fun fightState ->
                let b = Entities.DiscordInteractionResponseBuilder()

                let embed =
                    Entities.DiscordEmbedBuilder()
                        .WithColor(Entities.DiscordColor "#2f3136")
                        .WithTitle("Итог сражения!")
                        .WithDescription(sprintf "<@!%d> отказывается биться с <@!%d>! 👎" fightState.DefenderId fightState.AttackerId)
                        .Build()

                b.AddEmbed embed |> ignore

                e.Interaction.CreateResponseAsync(InteractionResponseType.UpdateMessage, b).GetAwaiter().GetResult()
            )

            true
        | _ ->
            RatingTable.componentInteractionCreateHandle
                (fun () -> m.PostAndReply (fun r -> GetGuildRating(e.Guild.Id, r)))
                client
                e
    else
        false
