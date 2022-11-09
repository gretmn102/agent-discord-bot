module SimpleQuiz.Main
open DSharpPlus
open FsharpMyExtension

open Types
open Extensions
open Model

type State = unit

type Request =
    | StartQuiz of quizName: string option

module Parser =
    open FParsec

    open DiscordMessage.Parser

    type 'Result Parser = Primitives.Parser<'Result, unit>

    module CommandNames =
        let quiz = "викторина"

    let pquiz =
        skipStringCI CommandNames.quiz
        .>> spaces
        >>. opt (many1Satisfy (fun _ -> true))

    let start f: _ Parser =
        choice [
            pquiz |>> StartQuiz
        ]
        >>= fun msg ->
            preturn (fun x -> f x msg)

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
                    | ComponentId.AnswerSelectedListId ->
                        if componentState.Data.OwnerId = e.User.Id then
                            match e.Values with
                            | [|answerId|] ->
                                match Map.tryFind componentState.Data.QuizId quizByQuestionId with
                                | Some questions ->
                                    match Map.tryFind componentState.Data.QuestionId questions with
                                    | Some question ->
                                        let send msg =
                                            let embed = Entities.DiscordEmbedBuilder()
                                            embed.Color <- Entities.Optional.FromValue(DiscordEmbed.backgroundColorDarkTheme)
                                            embed.Description <- msg
                                            let b = Entities.DiscordInteractionResponseBuilder()
                                            b.AddEmbed(embed.Build()) |> ignore
                                            awaiti <| e.Interaction.CreateResponseAsync(InteractionResponseType.UpdateMessage, b)
                                        if question.Correct = answerId then
                                            send "И это правильный ответ!"
                                        else
                                            send "К сожалению, это неправильный ответ."
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

    let startQuiz (addEmbed, addComponents) userId (quiz: Quiz) =
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

type Msg =
    | Request of EventArgs.MessageCreateEventArgs * Request

let actionReduce (e: EventArgs.MessageCreateEventArgs) (msg: Request) (state: State) =
    match msg with
    | StartQuiz quizName ->
        awaiti <| e.Channel.TriggerTypingAsync()

        match quizName with
        | None ->
            QuizSelectionUi.init e
        | Some quizName ->
            let res =
                quizes
                |> Map.tryPick (fun _ quiz ->
                    if quiz.Name = quizName then
                        Some quiz
                    else
                        None
                )

            match res with
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

let reduce (msg: Msg) (state: State): State =
    match msg with
    | Request(e, msg) ->
        actionReduce e msg state

let m =
    let init = ()

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
    QuizUi.handle client e
    || QuizSelectionUi.handle client e
