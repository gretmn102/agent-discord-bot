module AppsHub

open Types
open FsharpMyExtension
open FsharpMyExtension.Either


module Hub =
    type AppReq =
        | CyoaReq of Cyoa.Implementation.Msg
        | QuizReq of Quiz.Msg

    type AppAnswer =
        | CyoaAnswer of DSharpPlus.Entities.DiscordMessageBuilder
        | QuizAnswer of DSharpPlus.Entities.DiscordMessageBuilder

    type Err =
        | HasNotStartedYet
        | ThisIsNotYourApp

    type AppType =
        | CyoaType
        | QuizType

    type AppState =
        | CyoaState of Cyoa.Implementation.State<Cyoa.Scenario.LabelName,obj,obj>
        | QuizState of Quiz.State
    type State = Map<MessagePath, UserId * AppState>

    type Msg =
        {
            Update : (MessagePath -> Either<Err, (UserId -> Either<Err, AppType * (AppReq -> AppAnswer * State)>)>)
            Init : (UserId * AppType -> AppAnswer * (MessagePath -> State))
        }

    let exec (state:State) =
        {
            Update = fun path ->
                match Map.tryFind path state with
                | Some (ownerId, app) ->
                    Right (fun userId ->
                        if userId = ownerId then
                            match app with
                            | CyoaState cyoaState ->
                                Right (CyoaType, fun req ->
                                    match req with
                                    | CyoaReq msg ->
                                        let cyoaState = Cyoa.Implementation.update Cyoa.Scenario.interp Cyoa.scenario.Init msg cyoaState
                                        let cyoaAnswer = Cyoa.Implementation.gameView (fun _ _ -> failwith "not imp") cyoaState

                                        let state =
                                            match cyoaState.Game with
                                            | Cyoa.Core.End ->
                                                Map.remove path state
                                            | _ ->
                                                Map.add path (userId, CyoaState cyoaState) state

                                        CyoaAnswer cyoaAnswer, state
                                    | x -> failwithf "expected CyoaReq but %A" x
                                )
                            | QuizState quizState ->
                                Right (QuizType, fun req ->
                                    match req with
                                    | QuizReq msg ->
                                        let quizState = Quiz.update msg quizState
                                        let quizAnswer = Quiz.view quizState

                                        let state =
                                            match quizState.Now with
                                            | Quiz.Core.End _ ->
                                                Map.remove path state
                                            | _ ->
                                                Map.add path (userId, QuizState quizState) state

                                        QuizAnswer quizAnswer, state
                                    | x -> failwithf "expected QuizReq but %A" x
                                )
                        else
                            Left ThisIsNotYourApp
                    )
                | None -> Left HasNotStartedYet
            Init = fun (userId, appType) ->
                match appType with
                | CyoaType ->
                    let cyoaState = Cyoa.initState
                    let cyoaAnswer = Cyoa.Implementation.gameView (fun _ _ -> failwith "not imp") cyoaState
                    let f path =
                        let state = Map.add path (userId, CyoaState cyoaState) state
                        state

                    CyoaAnswer cyoaAnswer, f
                | QuizType ->
                    let quizState = Quiz.init (Quiz.loadQuiz ())
                    let quizAnswer = Quiz.view quizState
                    let f path =
                        let state = Map.add path (userId, QuizState quizState) state
                        state

                    QuizAnswer quizAnswer, f
        }

type Req =
    | Init of UserId * Hub.AppType * AsyncReplyChannel<DSharpPlus.Entities.DiscordMessageBuilder>
    | SecondInit of UserId * Hub.AppType * MessagePath
    | Update of MessagePath * UserId * DSharpPlus.EventArgs.ComponentInteractionCreateEventArgs * AsyncReplyChannel<Either<Hub.Err, DSharpPlus.Entities.DiscordMessageBuilder>>

let m =
    let exec (state:Hub.State) (msg:Req) =
        match msg with
        | Update (messagePath, userId, e, r) ->
            let f = Hub.exec state
            let res, state =
                match f.Update messagePath with
                | Right f ->
                    match f userId with
                    | Right (appType, f) ->
                        match appType with
                        | Hub.CyoaType ->
                            let msg =
                                match e.Id with
                                | Cyoa.Implementation.NextButtonId ->
                                    Cyoa.Implementation.Next
                                | Cyoa.Implementation.SelectMenuId ->
                                    Cyoa.Implementation.Choice (int e.Values.[0])
                                | x ->
                                    failwithf "expected Id = %s but %s" e.Id x
                            let answer, state = f (Hub.CyoaReq msg)
                            let answer =
                                match answer with
                                | Hub.CyoaAnswer answer -> answer
                                | x -> failwithf "expected CyoaAnswer but %A" x
                            Right answer, state
                        | Hub.QuizType ->
                            let msg =
                                match e.Id with
                                | Quiz.SelectMenuId ->
                                    Quiz.Select (int e.Values.[0])
                                | x ->
                                    failwithf "expected Id = %s but %s" e.Id x
                            let answer, state = f (Hub.QuizReq msg)
                            let answer =
                                match answer with
                                | Hub.QuizAnswer answer -> answer
                                | x -> failwithf "expected QuizAnswer but %A" x
                            Right answer, state

                    | Left(err) -> Left err, state
                | Left err ->
                    Left err, state
            r.Reply res
            state
        | Init(userId, appType, r) ->
            let f = Hub.exec state
            let answer, _ = f.Init (userId, appType)
            let answer =
                match answer with
                | Hub.CyoaAnswer answer
                | Hub.QuizAnswer answer -> answer
            r.Reply answer

            state
        | SecondInit(userId, appType, msgPath) ->
            let f = Hub.exec state
            let _, f = f.Init (userId, appType)
            f msgPath


    MailboxProcessor.Start (fun mail ->
        let rec loop (state:Hub.State) =
            async {
                let! msg = mail.Receive()
                let st =
                    try
                        exec state msg
                    with err ->
                        printfn "%A" err
                        state

                return! loop st
            }
        loop Map.empty
    )
module DiscordMessage =
    let removeComponents (msg:DSharpPlus.Entities.DiscordMessage) =
        let b = DSharpPlus.Entities.DiscordMessageBuilder()
        // embed, по-идеи, остается. Здесь главное подавить .Components
        b.Content <- msg.Content

        awaiti (msg.ModifyAsync(b))

let removeComponents (client:DSharpPlus.DiscordClient) channelId (messageId:MessageId) =
    let x = await (client.GetChannelAsync(channelId))
    try
        let msg =
            await (x.GetMessageAsync messageId)

        DiscordMessage.removeComponents msg
    with e -> ()

let resp (client:DSharpPlus.DiscordClient) (e: DSharpPlus.EventArgs.ComponentInteractionCreateEventArgs) =
    let msgPath =
        {
            GuildId = e.Guild.Id
            ChannelId = e.Channel.Id
            MessageId = e.Message.Id
        }

    match m.PostAndReply(fun r -> Update(msgPath, client.CurrentUser.Id, e, r)) with
    | Right res ->
        res
        |> e.Message.ModifyAsync
        |> awaiti

        e.Interaction.CreateResponseAsync(DSharpPlus.InteractionResponseType.UpdateMessage)
        |> fun x -> x.GetAwaiter().GetResult()
    | Left err ->
        match err with
        | Hub.HasNotStartedYet ->
            DiscordMessage.removeComponents e.Message

            let b = DSharpPlus.Entities.DiscordInteractionResponseBuilder()
            b.Content <- "Бот перезагружался, и игра слетела. Начните заново."
            b.IsEphemeral <- true

            e.Interaction.CreateResponseAsync(DSharpPlus.InteractionResponseType.ChannelMessageWithSource, b)
            |> fun x -> x.GetAwaiter().GetResult()
        | Hub.ThisIsNotYourApp ->
            let b = DSharpPlus.Entities.DiscordInteractionResponseBuilder()
            b.Content <- "Скорее всего, это чужая игра. Ваша игра [тут](), или начните новую." // TODO: ссылка на игру
            b.IsEphemeral <- true

            e.Interaction.CreateResponseAsync(DSharpPlus.InteractionResponseType.ChannelMessageWithSource, b)
            |> fun x -> x.GetAwaiter().GetResult()

let start appType (client:DSharpPlus.DiscordClient) (e: DSharpPlus.EventArgs.MessageCreateEventArgs) =
    let resp = m.PostAndReply(fun r -> Init(client.CurrentUser.Id, appType, r))

    let msg = await (client.SendMessageAsync (e.Channel, resp))

    let msgPath =
        {
            GuildId = msg.Channel.Guild.Id
            ChannelId = msg.Channel.Id
            MessageId = msg.Id
        }
    m.Post(SecondInit(client.CurrentUser.Id, appType, msgPath))
