module EggBattle.Main
open FsharpMyExtension
open FsharpMyExtension.Either
open DSharpPlus

open Types
open Extensions
open Model

type Request =
    | ChallengeToDuel of UserId option
    | CreateRatingTable

module Parser =
    open FParsec

    open DiscordMessage.Parser

    type 'Result Parser = Primitives.Parser<'Result, unit>

    let challangeToDuelName = "битва"

    let pchannelToDuel: _ Parser =
        skipStringCI challangeToDuelName >>. spaces
        >>. opt (puserMention <|> puint64)

    let start f: _ Parser =
        choice [
            pchannelToDuel |>> ChallengeToDuel
            skipStringCI "лидеры" >>% CreateRatingTable
        ]
        >>= fun msg ->
            preturn (fun x -> f x msg)

type State =
    {
        Rating: Rating.GuildUsers
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
    | Request of DiscordClient * EventArgs.MessageCreateEventArgs * Request
    | Fight of FightState * EventArgs.ComponentInteractionCreateEventArgs
    | GetState of AsyncReplyChannel<State>

module RatingTable =
    open Shared.Ui.Table

    type SortBy =
        | SortByWins = 0
        | SortByLoses = 1

    let initSetting getState: Setting<_, SortBy, _, (GuildId * State)> =
        {
            Id = "EggBattle"

            GetState = getState

            Title = fun _ _ -> "Битва на яйцах!"

            GetHeaders = fun sortBy ->
                match sortBy with
                | SortBy.SortByWins ->
                    [| "Игрок"; "Победы▼"; "Поражения" |]
                | SortBy.SortByLoses ->
                    [| "Игрок"; "Победы"; "Поражения▼" |]
                | x -> failwithf "RatingTable.SortBy %A" x

            GetItems = fun () (guildId, state) ->
                let state =
                    state.Rating.Cache
                    |> Seq.choose (fun (KeyValue(id, v)) ->
                        if id.GuildId = guildId then
                            Some v
                        else
                            None
                    )

                state
                |> Seq.toArray

            ItemsCountPerPage = 10

            SortBy = SortByContainer.Init [|
                SortBy.SortByWins, "Отсортировать по победам"
                SortBy.SortByLoses, "Отсортировать по поражениям"
            |]

            SortFunction = fun sortBy items ->
                match sortBy with
                | SortBy.SortByWins ->
                    Array.sortByDescending (fun x -> x.Data.Wins) items
                | SortBy.SortByLoses ->
                    Array.sortByDescending (fun x -> x.Data.Loses) items
                | x -> failwithf "RatingTable.SortBy %A" x

            MapFunction =
                fun _ i x ->
                    [|
                        sprintf "%d <@!%d>" i x.Id.UserId
                        string x.Data.Wins
                        string x.Data.Loses
                    |]
        }

    let createTable addComponents addEmbed state =
        createTable addComponents addEmbed 1 (None, ()) (initSetting state)

    let componentInteractionCreateHandle (client: DiscordClient) (e: EventArgs.ComponentInteractionCreateEventArgs) getState =
        let getState () =
            let state: State = getState ()
            e.Guild.Id, state

        componentInteractionCreateHandle client e (initSetting getState)

[<Literal>]
let YesButtonId = "EggBattleYesButtonId"

[<Literal>]
let NoButtonId = "EggBattleNoButtonId"

let r = new System.Random()

let reduce msg (state: State) =
    match msg with
    | Request(client, e, r) ->
        match r with
        | ChallengeToDuel userId ->
            awaiti <| e.Channel.TriggerTypingAsync()

            let send msg =
                let b =
                    Entities.DiscordMessageBuilder()
                        .WithContent(msg)

                awaiti <| e.Channel.SendMessageAsync(b)

                state

            let getTargetUserId next =
                match userId with
                | Some userId -> next userId
                | None ->
                    match e.Message.ReferencedMessage with
                    | null ->
                        sprintf "<@%d>, укажи или процитируй любое сообщение противника, чтобы вызвать его на бой! <:catAttack:1029835643834077315>" e.Author.Id
                        |> send
                    | referencedMessage ->
                        next referencedMessage.Author.Id

            let checkUserAreNotCurrentUser targetUserId next =
                if e.Author.Id = targetUserId then
                    sprintf "<@%d>, нельзя самого себя вызывать на бой!" e.Author.Id
                    |> send
                else
                    next ()

            let checkUserAreNotCurrentBot targetUserId next =
                if client.CurrentUser.Id = targetUserId then
                    sprintf "<@%d>, со мной лучше не драться, кожанный мешок :robot:" e.Author.Id
                    |> send
                else
                    next ()

            let getTargetUser targetUserId next =
                let targetUser =
                    try
                        await <| e.Guild.GetMemberAsync targetUserId
                        |> Some
                    with _ ->
                        None

                match targetUser with
                | Some user -> next user
                | None ->
                    sprintf "<@%d>, пользователь с %d ID не найден" e.Author.Id targetUserId
                    |> send

            let checkTargetAreNotBot (target: Entities.DiscordUser) next =
                if target.IsBot then
                    sprintf "<@%d>, <@%d> является ботом, а боты драться не умеют :robot:" e.Author.Id target.Id
                    |> send
                else
                    next ()

            getTargetUserId <| fun targetUserId ->
            checkUserAreNotCurrentUser targetUserId <| fun () ->
            checkUserAreNotCurrentBot targetUserId <| fun () ->
            getTargetUser targetUserId <| fun targetUser ->
            checkTargetAreNotBot targetUser <| fun () ->
                let b = Entities.DiscordMessageBuilder()

                b.Embed <-
                    Entities.DiscordEmbedBuilder(
                        Color = Entities.Optional.FromValue(DiscordEmbed.backgroundColorDarkTheme),
                        Title = "Вызов на дуэль!",
                        Description = sprintf "<@%d>, <@%d> вызывает тебя на бой на 🥚! Соглашаешься?! <:angry:927633404353196113>" targetUserId e.Author.Id
                    ).Build()

                let applyToPartialState str =
                    let partialStateStr =
                        { AttackerId = e.Author.Id; DefenderId = targetUserId }
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

                state
        | CreateRatingTable ->
            awaiti <| e.Channel.TriggerTypingAsync()

            let b = Entities.DiscordMessageBuilder()

            RatingTable.createTable b.AddComponents b.AddEmbed (fun () -> e.Guild.Id, state)

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
            state.Rating
            |> Rating.GuildUsers.set
                (Rating.Id.create guildId winnerId)
                (fun data ->
                    { data with
                        Wins = data.Wins + 1
                    }
                )
            |> Rating.GuildUsers.set
                (Rating.Id.create guildId loserId)
                (fun data ->
                    { data with
                        Loses = data.Loses + 1
                    }
                )

        let b = Entities.DiscordInteractionResponseBuilder()

        let embed =
            Entities.DiscordEmbedBuilder()
                .WithColor(DiscordEmbed.backgroundColorDarkTheme)
                .WithTitle("Итог сражения!")
                .WithDescription(sprintf "И в схватке на 🥚 между <@!%d> и <@!%d> побеждает... <@!%d>! 🎉" fightState.AttackerId fightState.DefenderId winnerId)
                .Build()

        b.AddEmbed embed |> ignore

        awaiti <| e.Interaction.CreateResponseAsync(InteractionResponseType.UpdateMessage, b)

        { state with
            Rating = ratingState
        }

    | GetState r ->
        r.Reply state

        state

let m =
    let init = {
        Rating = Rating.GuildUsers.init "EggBattleRatings" Db.database
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

let exec: MessageCreateEventHandler Parser.Parser =
    Parser.start (fun (client: DiscordClient, e: EventArgs.MessageCreateEventArgs) msg ->
        m.Post (Request (client, e, msg))
    )

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
                        .WithColor(DiscordEmbed.backgroundColorDarkTheme)
                        .WithTitle("Итог сражения!")
                        .WithDescription(sprintf "<@!%d> отказывается биться с <@!%d>! 👎" fightState.DefenderId fightState.AttackerId)
                        .Build()

                b.AddEmbed embed |> ignore

                awaiti <| e.Interaction.CreateResponseAsync(InteractionResponseType.UpdateMessage, b)
            )

            true
        | _ ->
            RatingTable.componentInteractionCreateHandle
                client
                e
                (fun () -> m.PostAndReply GetState)
    else
        false
