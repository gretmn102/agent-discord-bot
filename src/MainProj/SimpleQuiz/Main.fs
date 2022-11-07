module SimpleQuiz.Main
open DSharpPlus
open FsharpMyExtension

open Types
open Extensions
open Model

type State = unit

type Request =
    | StartQuiz

module Parser =
    open FParsec

    open DiscordMessage.Parser

    type 'Result Parser = Primitives.Parser<'Result, unit>

    module CommandNames =
        let quiz = "викторина"

    let pquiz =
        skipStringCI CommandNames.quiz

    let start: _ Parser =
        choice [
            pquiz >>% StartQuiz
        ]

module QuizUi =
    type ComponentId =
        | AnswerSelectedListId = 0

    type Data =
        {
            OwnerId: UserId
            QuestionId: QuestionId
        }

    type ComponentState = Interaction.ComponentState<ComponentId, Data>

    let messageTypeId = "SimpleQuizUI"

    type InitOptions =
        {
            Question: Question
        }
        static member create question =
            {
                Question = question
            }

    let init (options: InitOptions) (e: EventArgs.MessageCreateEventArgs) =
        let userId = e.Author.Id

        let embed = Entities.DiscordEmbedBuilder()
        embed.Color <- Entities.Optional.FromValue(DiscordEmbed.backgroundColorDarkTheme)
        embed.Description <- sprintf "<@%d>, %s" userId options.Question.Description

        let b = Entities.DiscordMessageBuilder()
        b.Embed <- embed.Build()

        let componentState =
            let state: ComponentState =
                {
                    Id = messageTypeId
                    ComponentId = ComponentId.AnswerSelectedListId
                    Data = {
                        OwnerId = userId
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
                    | ComponentId.AnswerSelectedListId ->
                        if componentState.Data.OwnerId = e.User.Id then
                            match e.Values with
                            | [|answerId|] ->
                                match Map.tryFind componentState.Data.QuestionId quizByQuestionId with
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

type Msg =
    | Request of EventArgs.MessageCreateEventArgs * Request

let r = new System.Random()

module Actions =
    let startQuiz (e: EventArgs.MessageCreateEventArgs) =
        let question =
            QuizUi.InitOptions.create
                quiz.[r.Next(0, quiz.Length)]
        QuizUi.init question e

let actionReduce (e: EventArgs.MessageCreateEventArgs) (msg: Request) (state: State) =
    match msg with
    | StartQuiz ->
        awaiti <| e.Channel.TriggerTypingAsync()

        Actions.startQuiz e

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

let exec e msg =
    m.Post (Request (e, msg))

let componentInteractionCreateHandle client e =
    QuizUi.handle client e
