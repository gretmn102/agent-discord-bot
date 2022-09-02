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
    open Shared.Ui.Table

    [<Struct>]
    type SortBy = SortByWins

    let initSetting (state: Map<UserId, Rating.Data>): Setting<_, SortBy> =
        {
            Id = "EggBattle"

            Title = "Битва на яйцах!"

            GetHeaders = fun sortBy ->
                match sortBy with
                | SortByWins ->
                    [| "Игрок"; "Победы▼"; "Поражения" |]

            GetItems = fun () ->
                state
                |> Map.toArray

            ItemsCountPerPage = 10

            SortBy = SortByWins

            SortFunction = fun sortBy items ->
                match sortBy with
                | SortByWins ->
                    Array.sortByDescending (fun (_, data) -> data.Wins) items

            MapFunction =
                fun i (userId, data) ->
                    [|
                        sprintf "%d <@!%d>" i userId
                        string data.Wins
                        string data.Loses
                    |]
        }

    let createTable addComponents addEmbed page state =
        createTable addComponents addEmbed page (initSetting state)

    let componentInteractionCreateHandle getState (client: DiscordClient) (e: EventArgs.ComponentInteractionCreateEventArgs) =
        let state = getState ()
        componentInteractionCreateHandle client e (initSetting state)

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
            awaiti <| e.Channel.TriggerTypingAsync()

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
            awaiti <| e.Channel.TriggerTypingAsync()

            let b = Entities.DiscordMessageBuilder()

            match Map.tryFind e.Guild.Id state.Rating with
            | Some guildRating ->
                RatingTable.createTable b.AddComponents b.AddEmbed 1 guildRating
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

        awaiti <| e.Interaction.CreateResponseAsync(InteractionResponseType.UpdateMessage, b)

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

                    awaiti <| e.Interaction.CreateResponseAsync(InteractionResponseType.ChannelMessageWithSource, b)

            | Left errMsg ->
                // remove components
                let msg = e.Message
                let b = Entities.DiscordInteractionResponseBuilder()
                // necessary because throw `System.ArgumentException: You must specify content, an embed, a sticker, or at least one file.`
                b.AddEmbeds msg.Embeds |> ignore
                b.Content <- msg.Content
                awaiti <| e.Interaction.CreateResponseAsync(InteractionResponseType.UpdateMessage, b)

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

                awaiti <| e.Interaction.CreateResponseAsync(InteractionResponseType.UpdateMessage, b)
            )

            true
        | _ ->
            RatingTable.componentInteractionCreateHandle
                (fun () -> m.PostAndReply (fun r -> GetGuildRating(e.Guild.Id, r)))
                client
                e
    else
        false
