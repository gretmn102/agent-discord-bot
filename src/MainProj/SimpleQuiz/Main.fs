module SimpleQuiz.Main
open DSharpPlus
open FsharpMyExtension

open Types
open Extensions
open Model

type State = {
    Players: Players.GuildData
}

type Request =
    | StartQuiz of quizName: string option
    | StartRating of quizName: string

module Parser =
    open FParsec

    open DiscordMessage.Parser

    type 'Result Parser = Primitives.Parser<'Result, unit>

    module CommandNames =
        let rating = "викторинаРейтинг"
        let quiz = "викторина"

    let pquiz =
        skipStringCI CommandNames.quiz
        .>> spaces
        >>. opt (many1Satisfy (fun _ -> true))

    let prating =
        skipStringCI CommandNames.rating
        .>> spaces
        >>. many1Satisfy (fun _ -> true)

    let start f: _ Parser =
        choice [
            prating |>> StartRating
            pquiz |>> StartQuiz
        ]
        >>= fun msg ->
            preturn (fun x -> f x msg)

type ScoreType = Win | Lose

module QuizUi =
    type ComponentId =
        | AnswerSelectedListId = 0

    type Data =
        {
            [<Newtonsoft.Json.JsonProperty("O")>]
            OwnerId: UserId
            [<Newtonsoft.Json.JsonProperty("Qz")>]
            QuizId: QuizId
            [<Newtonsoft.Json.JsonProperty("Qs")>]
            QuestionId: QuestionId
        }

    type ComponentState = Interaction.ComponentState<ComponentId, Data>

    let messageTypeId = "SimpleQuizUI"

    type InitOptions =
        {
            UserId: UserId
            QuizId: QuizId
            Question: Question
        }
        static member create userId quizId question =
            {
                UserId = userId
                QuizId = quizId
                Question = question
            }

    let init (options: InitOptions) (addEmbed, addComponents) =
        let embed = Entities.DiscordEmbedBuilder()
        embed.Color <- Entities.Optional.FromValue(DiscordEmbed.backgroundColorDarkTheme)
        embed.Description <- sprintf "<@%d>, %s" options.UserId options.Question.Description

        addEmbed (embed.Build())

        let componentState =
            let state: ComponentState =
                {
                    Id = messageTypeId
                    ComponentId = ComponentId.AnswerSelectedListId
                    Data = {
                        OwnerId = options.UserId
                        QuizId = options.QuizId
                        QuestionId = options.Question.Id
                    }
                }
            state
            |> ComponentState.Serialize

        let options =
            Array.append [|options.Question.Correct|] options.Question.Others
            |> List.ofArray
            |> List.shuffle
            |> List.mapi (fun i item ->
                Entities.DiscordSelectComponentOption(item, item)
            )

        let c = Entities.DiscordSelectComponent(componentState, "Выбери ответ...", options)
        addComponents (c :> Entities.DiscordComponent)
        ()

    let handle (client: DiscordClient) (e: EventArgs.ComponentInteractionCreateEventArgs) addWin =
        let restartComponent errMsg =
            DiscordMessage.Ext.clearComponents e.Message

            let b = Entities.DiscordInteractionResponseBuilder()
            b.Content <-
                [
                    sprintf "Вызовите комманду `.%s` еще раз, потому что-то пошло не так:" Parser.CommandNames.quiz
                    "```"
                    sprintf "%s" errMsg
                    "```"
                ] |> String.concat "\n"
            b.IsEphemeral <- true
            awaiti <| e.Interaction.CreateResponseAsync(InteractionResponseType.ChannelMessageWithSource, b)

        if e.Message.Author.Id = client.CurrentUser.Id then
            match ComponentState.TryDeserialize messageTypeId e.Id with
            | Some res ->
                match res with
                | Ok (componentState: ComponentState) ->
                    match componentState.ComponentId with
                    | ComponentId.AnswerSelectedListId ->
                        let userId = e.User.Id
                        if componentState.Data.OwnerId = userId then
                            match e.Values with
                            | [|answerId|] ->
                                match Map.tryFind componentState.Data.QuizId quizByQuestionId with
                                | Some quiz ->
                                    match Map.tryFind componentState.Data.QuestionId quiz.Questions with
                                    | Some question ->
                                        let send msg =
                                            let embed = Entities.DiscordEmbedBuilder()
                                            embed.Color <- Entities.Optional.FromValue(DiscordEmbed.backgroundColorDarkTheme)
                                            embed.Description <- msg
                                            let b = Entities.DiscordInteractionResponseBuilder()
                                            b.AddEmbed(embed.Build()) |> ignore
                                            awaiti <| e.Interaction.CreateResponseAsync(InteractionResponseType.UpdateMessage, b)

                                        let ratingPosition scoreType = addWin ((Players.Id.create userId componentState.Data.QuizId), scoreType)

                                        if question.Correct = answerId then
                                            sprintf "<@%d>, и это правильный ответ! 🥳\nТы на %d месте в рейтинге по \"%s\" викторине!"
                                                userId
                                                (ratingPosition Win)
                                                quiz.Name
                                            |> send
                                        else
                                            sprintf "<@%d>, к сожалению, это неправильный ответ.\nТы на %d месте в рейтинге по \"%s\" викторине"
                                                userId
                                                (ratingPosition Lose)
                                                quiz.Name
                                            |> send
                                    | None ->
                                        sprintf "not found in %s question ID in questions" componentState.Data.QuestionId
                                        |> restartComponent
                                | None ->
                                    sprintf "not found in %s quiz ID in questions" componentState.Data.QuizId
                                    |> restartComponent
                            | xs ->
                                sprintf "expected `e.Values` is [|answerId|] but %A" xs
                                |> restartComponent
                        else
                            let b = Entities.DiscordInteractionResponseBuilder()
                            b.Content <-
                                sprintf "На вопрос отвечает <@%d>. Чтобы поучаствовать в викторине, введите `.%s`"
                                    componentState.Data.OwnerId
                                    Parser.CommandNames.quiz
                            b.IsEphemeral <- true
                            awaiti <| e.Interaction.CreateResponseAsync(InteractionResponseType.ChannelMessageWithSource, b)
                    | x ->
                        sprintf "expected data.ComponentId but %A" x
                        |> restartComponent

                    true
                | Error errMsg ->
                    restartComponent errMsg

                    true
            | _ -> false
        else
            false

module Actions =
    let r = new System.Random()

    let startQuiz (addEmbed, addComponents) userId (quiz: QuizArray) =
        let questions = quiz.Questions
        let initOptions =
            QuizUi.InitOptions.create
                userId
                quiz.Id
                questions.[r.Next(0, questions.Length)]
        QuizUi.init initOptions (addEmbed, addComponents)

module QuizSelectionUi =
    type ComponentId =
        | QuizSelectionListId = 0

    type Data =
        {
            [<Newtonsoft.Json.JsonProperty("O")>]
            OwnerId: UserId
        }

    type ComponentState = Interaction.ComponentState<ComponentId, Data>

    let messageTypeId = "SimpleQuizSelectionUI"

    let init (e: EventArgs.MessageCreateEventArgs) =
        let userId = e.Author.Id

        let embed = Entities.DiscordEmbedBuilder()
        embed.Color <- Entities.Optional.FromValue(DiscordEmbed.backgroundColorDarkTheme)
        embed.Description <- sprintf "<@%d>, выбери викторину:" userId

        let b = Entities.DiscordMessageBuilder()
        b.Embed <- embed.Build()

        let componentState =
            let state: ComponentState =
                {
                    Id = messageTypeId
                    ComponentId = ComponentId.QuizSelectionListId
                    Data = {
                        OwnerId = e.Author.Id
                    }
                }
            state
            |> ComponentState.Serialize

        let options =
            quizes
            |> Seq.map (fun (KeyValue(quizId, quiz)) ->
                Entities.DiscordSelectComponentOption(quiz.Name, quizId)
            )

        let c = Entities.DiscordSelectComponent(componentState, "Выбери викторину...", options)
        b.AddComponents c |> ignore

        awaiti <| e.Channel.SendMessageAsync b

    let handle (client: DiscordClient) (e: EventArgs.ComponentInteractionCreateEventArgs) =
        let restartComponent errMsg =
            DiscordMessage.Ext.clearComponents e.Message

            let b = Entities.DiscordInteractionResponseBuilder()
            b.Content <-
                [
                    sprintf "Вызовите комманду `.%s` еще раз, потому что-то пошло не так:" Parser.CommandNames.quiz
                    "```"
                    sprintf "%s" errMsg
                    "```"
                ] |> String.concat "\n"
            b.IsEphemeral <- true
            awaiti <| e.Interaction.CreateResponseAsync(InteractionResponseType.ChannelMessageWithSource, b)

        if e.Message.Author.Id = client.CurrentUser.Id then
            match ComponentState.TryDeserialize messageTypeId e.Id with
            | Some res ->
                match res with
                | Ok (componentState: ComponentState) ->
                    match componentState.ComponentId with
                    | ComponentId.QuizSelectionListId ->
                        if componentState.Data.OwnerId = e.User.Id then
                            match e.Values with
                            | [|quizId|] ->
                                match Map.tryFind quizId quizes with
                                | Some quiz ->
                                    let b = Entities.DiscordInteractionResponseBuilder()

                                    Actions.startQuiz
                                        (b.AddEmbed >> ignore, b.AddComponents >> ignore)
                                        e.User.Id
                                        quiz

                                    awaiti <| e.Interaction.CreateResponseAsync(InteractionResponseType.UpdateMessage, b)

                                | None ->
                                    sprintf "not found in %s quiz ID in quizes" quizId
                                    |> restartComponent
                            | xs ->
                                sprintf "expected `e.Values` is [|answerId|] but %A" xs
                                |> restartComponent
                        else
                            let b = Entities.DiscordInteractionResponseBuilder()
                            b.Content <-
                                sprintf "На вопрос отвечает <@%d>. Чтобы поучаствовать в викторине, введите `.%s`"
                                    componentState.Data.OwnerId
                                    Parser.CommandNames.quiz
                            b.IsEphemeral <- true
                            awaiti <| e.Interaction.CreateResponseAsync(InteractionResponseType.ChannelMessageWithSource, b)
                    | x ->
                        sprintf "expected data.ComponentId but %A" x
                        |> restartComponent

                | Error errMsg ->
                    restartComponent errMsg

                true
            | _ -> false
        else
            false

module RatingTableUi =
    open Shared.Ui.Table

    type SortBy =
        | SortByIndex = 0
        | SortByWins = 1
        | SortByLoses = 2

    let initSetting getState: Setting<_, SortBy, QuizId, Players.GuildData> =
        {
            Id = sprintf "RatingTableUi"

            GetState = getState

            Title = fun quizId _ ->
                match Map.tryFind quizId quizes with
                | Some quiz ->
                    quiz.Name
                | None ->
                    "Неизвестная викторина"

            GetHeaders =
                let mark targetIndex =
                    let markChar = "▼"

                    [| "Учасники"; "Угадано"; "Не угадано" |]
                    |> Array.mapi (fun i headerName ->
                        if i = targetIndex then
                            sprintf "%s%s" headerName markChar
                        else
                            headerName
                    )

                fun sortBy ->
                match sortBy with
                | SortBy.SortByIndex ->
                    mark 0
                | SortBy.SortByWins ->
                    mark 1
                | SortBy.SortByLoses ->
                    mark 2
                | x ->
                    failwithf "MostActiveTable.SortBy %A" x

            GetItems = fun quizId state ->
                let players =
                    Players.GuildData.tryFindQuizPlayers quizId state
                    |> Option.defaultValue Map.empty

                players
                |> Map.toArray

            ItemsCountPerPage = 10

            SortBy = SortByContainer.Init [|
                SortBy.SortByWins, "Отсортировать по кол-ву правильных ответов"
                SortBy.SortByLoses, "Отсортировать по кол-ву неправильных ответов"
                SortBy.SortByIndex, "Отсортировать по общ. индексу"
            |]

            SortFunction = fun sortBy items ->
                match sortBy with
                | SortBy.SortByWins ->
                    Array.sortByDescending (fun (_, data) -> data.Data.Wins) items
                | SortBy.SortByLoses ->
                    Array.sortByDescending (fun (_, data) -> data.Data.Loses) items
                | SortBy.SortByIndex -> items
                | x ->
                    failwithf "MostActiveTable.SortBy %A" x

            MapFunction =
                fun _ i (userId, user) ->
                    [|
                        sprintf "%d <@!%d>" i userId
                        string user.Data.Wins
                        string user.Data.Loses
                    |]
        }

    let createTable
        (addEmbed, (addComponents: Entities.DiscordComponent [] -> _))
        (quiz: Quiz<_>, userRanks) =

        createTable addComponents addEmbed 1 (None, quiz.Id) (initSetting userRanks)

    let componentInteractionCreateHandle (client: DiscordClient) (e: EventArgs.ComponentInteractionCreateEventArgs) state =
        componentInteractionCreateHandle client e (initSetting state)

type Msg =
    | Request of EventArgs.MessageCreateEventArgs * Request
    /// returned player position in rating
    | AddScore of AsyncReplyChannel<int> * (Players.Id * ScoreType)
    | GetPlayers of AsyncReplyChannel<Players.GuildData>

let actionReduce (e: EventArgs.MessageCreateEventArgs) (msg: Request) (state: State) =
    let tryFindQuizByName quizName =
        quizes
        |> Map.tryPick (fun _ quiz ->
            if quiz.Name = quizName then
                Some quiz
            else
                None
        )

    match msg with
    | StartQuiz quizName ->
        awaiti <| e.Channel.TriggerTypingAsync()

        match quizName with
        | None ->
            QuizSelectionUi.init e
        | Some quizName ->
            match tryFindQuizByName quizName with
            | Some quiz ->
                let b = Entities.DiscordMessageBuilder()
                let addEmbed c = b.Embed <- c
                Actions.startQuiz
                    (addEmbed, b.AddComponents >> ignore)
                    e.Author.Id
                    quiz
                awaiti <| e.Channel.SendMessageAsync b

            | None ->
                let send msg =
                    let embed = Entities.DiscordEmbedBuilder()
                    embed.Color <- Entities.Optional.FromValue(DiscordEmbed.backgroundColorDarkTheme)
                    embed.Description <- msg
                    let b = Entities.DiscordMessageBuilder()
                    b.Embed <- embed.Build()
                    awaiti <| e.Channel.SendMessageAsync b

                sprintf "Не найдена викторина с таким названием."
                |> send

        state

    | StartRating quizName ->
        awaiti <| e.Channel.TriggerTypingAsync()

        match tryFindQuizByName quizName with
        | Some quiz ->
            let b = Entities.DiscordMessageBuilder()

            RatingTableUi.createTable
                ((fun c -> b.Embed <- c), b.AddComponents >> ignore)
                (quiz, fun () -> state.Players)

            awaiti <| e.Channel.SendMessageAsync b

        | None ->
            let send msg =
                let embed = Entities.DiscordEmbedBuilder()
                embed.Color <- Entities.Optional.FromValue(DiscordEmbed.backgroundColorDarkTheme)
                embed.Description <- msg
                let b = Entities.DiscordMessageBuilder()
                b.Embed <- embed.Build()
                awaiti <| e.Channel.SendMessageAsync b

            sprintf "Не найдена викторина с таким названием."
            |> send

        state

let reduce (msg: Msg) (state: State): State =
    match msg with
    | Request(e, msg) ->
        actionReduce e msg state
    | AddScore(r, (id, scoreType)) ->
        let players =
            let f (x: Players.MainData) =
                match scoreType with
                | Win ->
                    { x with Wins = x.Wins + 1 }
                | Lose ->
                    { x with Loses = x.Wins + 1 }

            Players.GuildData.set id f state.Players

        let rating =
            match Players.GuildData.tryFindQuizPlayers id.QuizId players with
            | Some players ->
                let ratingPosition =
                    players
                    |> Seq.sortByDescending (fun (KeyValue(_, p)) -> p.Data.Wins)
                    |> Seq.indexed
                    |> Seq.tryFind (fun (_, (KeyValue(userId, _))) -> userId = id.UserId)

                match ratingPosition with
                | Some (pos, _) -> pos + 1
                | None ->
                    failwith "not found player in rating position"
            | None ->
                failwith "not found players in quizes"

        r.Reply rating

        { state with
            Players = players
        }
    | GetPlayers r ->
        r.Reply state.Players

        state

let m =
    let init: State = {
        Players = Players.GuildData.init Db.database
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
        m.Post (Request (e, msg))
    )

let componentInteractionCreateHandle client e =
    QuizUi.handle client e (fun x ->
        m.PostAndReply (fun r -> AddScore(r, x))
    )
    || QuizSelectionUi.handle client e
    || RatingTableUi.componentInteractionCreateHandle client e (fun () ->
        m.PostAndReply GetPlayers
    )
