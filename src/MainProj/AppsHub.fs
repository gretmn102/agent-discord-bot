module AppsHub

open Types
open FsharpMyExtension
open FsharpMyExtension.Either


module Hub =
    type AppReq =
        | CyoaReq of Cyoa.Implementation.Msg
        | SomeGirlsQuizReq of Cyoa.Implementation.Msg
        | QuizWithMultiChoicesReq of Cyoa.Implementation.Msg
        | QuizReq of Quiz.Msg
        | BallotBoxReq

    type AppAnswer =
        | CyoaAnswer of DSharpPlus.Entities.DiscordMessageBuilder
        | SomeGirlsQuizAnswer of DSharpPlus.Entities.DiscordMessageBuilder
        | QuizAnswer of DSharpPlus.Entities.DiscordMessageBuilder
        | QuizWithMultiChoicesAnswer of DSharpPlus.Entities.DiscordMessageBuilder
        | BallotBoxAnswer of Types.ResultView
    type Err =
        | HasNotStartedYet
        | ThisIsNotYourApp
        | OtherErr of DSharpPlus.Entities.DiscordMessageBuilder

    type InitApp =
        | InitCyoa
        | InitSomeGirlsQuiz
        | InitQuiz
        | InitQuizWithMultiChoices
        | InitBallotBox of description:string * choises:string list

    type AppType =
        | CyoaType
        | SomeGirlsQuizType
        | QuizWithMultiChoicesType
        | QuizType
        | BallotBoxType

    type AppState =
        | CyoaState of Cyoa.Implementation.State<Cyoa.Scenario.LabelName,obj,obj>
        | SomeGirlsQuizState of Cyoa.Implementation.State<Cyoa.Scenario2.LabelName,obj,obj>
        | QuizWithMultiChoicesState of Cyoa.Implementation.State<Cyoa.QuizWithMultipleChoice.LabelName,obj,obj>
        | QuizState of Quiz.State
        | BallotBoxState of BallotBox.State
    type State = Map<MessagePath, UserId * AppState>

    type Msg =
        {
            Update : (MessagePath -> Either<Err, (DSharpPlus.EventArgs.ComponentInteractionCreateEventArgs -> Either<Err, AppType * (AppReq -> AppAnswer * State)>)>)
            Init : (UserId * InitApp -> AppAnswer * (MessagePath -> State))
        }

    let exec (state:State) =
        {
            Update = fun path ->
                match Map.tryFind path state with
                | Some (ownerId, app) ->
                    Right (fun e ->
                        let userId = e.User.Id
                        match app with
                        | CyoaState cyoaState ->
                            if userId = ownerId then
                                Right (CyoaType, fun req ->
                                    match req with
                                    | CyoaReq msg ->
                                        let cyoaState = Cyoa.Implementation.update Cyoa.Scenario.interp Cyoa.Scenario.all.Init msg cyoaState
                                        let cyoaAnswer = Cyoa.Implementation.gameView (fun _ _ -> failwith "not imp") cyoaState

                                        let state =
                                            match cyoaState.Game with
                                            | Cyoa.Core.End
                                            | Cyoa.Core.PrintEnd _ ->
                                                Map.remove path state
                                            | _ ->
                                                Map.add path (userId, CyoaState cyoaState) state

                                        CyoaAnswer cyoaAnswer, state
                                    | x -> failwithf "expected CyoaReq but %A" x
                                )
                            else
                                Left ThisIsNotYourApp
                        | SomeGirlsQuizState cyoaState ->
                            if userId = ownerId then
                                Right (SomeGirlsQuizType, fun req ->
                                    match req with
                                    | SomeGirlsQuizReq msg ->
                                        let cyoaState = Cyoa.Implementation.update Cyoa.Scenario2.interp Cyoa.Scenario2.all.Init msg cyoaState
                                        let cyoaAnswer = Cyoa.Implementation.gameView (fun _ _ -> failwith "not imp") cyoaState

                                        let state =
                                            match cyoaState.Game with
                                            | Cyoa.Core.End
                                            | Cyoa.Core.PrintEnd _ ->
                                                Map.remove path state
                                            | _ ->
                                                Map.add path (userId, SomeGirlsQuizState cyoaState) state

                                        SomeGirlsQuizAnswer cyoaAnswer, state
                                    | x -> failwithf "expected SomeGirlsQuizReq but %A" x
                                )
                            else
                                Left ThisIsNotYourApp
                        | QuizWithMultiChoicesState cyoaState ->
                            if userId = ownerId then
                                Right (QuizWithMultiChoicesType, fun req ->
                                    match req with
                                    | QuizWithMultiChoicesReq msg ->
                                        let cyoaState = Cyoa.Implementation.update Cyoa.QuizWithMultipleChoice.interp Cyoa.QuizWithMultipleChoice.all.Init msg cyoaState
                                        let cyoaAnswer = Cyoa.Implementation.gameView (fun _ _ -> failwith "not imp") cyoaState

                                        let state =
                                            match cyoaState.Game with
                                            | Cyoa.Core.End
                                            | Cyoa.Core.PrintEnd _ ->
                                                Map.remove path state
                                            | _ ->
                                                Map.add path (userId, QuizWithMultiChoicesState cyoaState) state

                                        QuizWithMultiChoicesAnswer cyoaAnswer, state
                                    | x -> failwithf "expected QuizWithMultiChoicesReq but %A" x
                                )
                            else
                                Left ThisIsNotYourApp
                        | QuizState quizState ->
                            if userId = ownerId then
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
                        | BallotBoxState ballotBoxState ->
                            Right (BallotBoxType, fun req -> // TODO: it is obvious that one level of abstraction is clearly superfluous
                                let x, ballotBoxState = BallotBox.update2 e ballotBoxState
                                let state =
                                    if ballotBoxState.IsPublic then
                                        Map.remove path state
                                    else
                                        Map.add path (userId, BallotBoxState ballotBoxState) state
                                BallotBoxAnswer x, state
                            )
                    )
                | None -> Left HasNotStartedYet
            Init = fun (userId, appType) ->
                match appType with
                | InitCyoa ->
                    let cyoaState = Cyoa.Scenario.initState
                    let cyoaAnswer = Cyoa.Implementation.gameView (fun _ _ -> failwith "not imp") cyoaState
                    let f path =
                        let state = Map.add path (userId, CyoaState cyoaState) state
                        state

                    CyoaAnswer cyoaAnswer, f
                | InitSomeGirlsQuiz ->
                    let cyoaState = Cyoa.Scenario2.initState
                    let cyoaAnswer = Cyoa.Implementation.gameView (fun _ _ -> failwith "not imp") cyoaState
                    let f path =
                        let state = Map.add path (userId, SomeGirlsQuizState cyoaState) state
                        state

                    SomeGirlsQuizAnswer cyoaAnswer, f
                | InitQuizWithMultiChoices ->
                    let cyoaState = Cyoa.QuizWithMultipleChoice.initState
                    let cyoaAnswer = Cyoa.Implementation.gameView (fun _ _ -> failwith "not imp") cyoaState
                    let f path =
                        let state = Map.add path (userId, QuizWithMultiChoicesState cyoaState) state
                        state

                    QuizWithMultiChoicesAnswer cyoaAnswer, f
                | InitQuiz ->
                    let quizState = Quiz.init (Quiz.loadQuiz ())
                    let quizAnswer = Quiz.view quizState
                    let f path =
                        let state = Map.add path (userId, QuizState quizState) state
                        state

                    QuizAnswer quizAnswer, f
                | InitBallotBox(description, choices) ->
                    let (answer, ballotBoxState) = BallotBox.init2 userId description choices
                    let f path =
                        let state = Map.add path (userId, BallotBoxState ballotBoxState) state
                        state

                    BallotBoxAnswer answer, f

        }

type Req =
    | Init of UserId * Hub.InitApp * AsyncReplyChannel<DSharpPlus.Entities.DiscordMessageBuilder>
    | SecondInit of UserId * Hub.InitApp * MessagePath
    | Update of MessagePath * DSharpPlus.EventArgs.ComponentInteractionCreateEventArgs * AsyncReplyChannel<Either<Hub.Err, DSharpPlus.Entities.DiscordMessageBuilder>>

let m =
    let exec (state:Hub.State) (msg:Req) =
        match msg with
        | Update (messagePath, e, r) ->
            let f = Hub.exec state
            let res, state =
                match f.Update messagePath with
                | Right f ->
                    match f e with
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
                        | Hub.SomeGirlsQuizType ->
                            let msg =
                                match e.Id with
                                | Cyoa.Implementation.NextButtonId ->
                                    Cyoa.Implementation.Next
                                | Cyoa.Implementation.SelectMenuId ->
                                    Cyoa.Implementation.Choice (int e.Values.[0])
                                | x ->
                                    failwithf "expected Id = %s but %s" e.Id x
                            let answer, state = f (Hub.SomeGirlsQuizReq msg)
                            let answer =
                                match answer with
                                | Hub.SomeGirlsQuizAnswer answer -> answer
                                | x -> failwithf "expected SomeGirlsQuizAnswer but %A" x
                            Right answer, state
                        | Hub.QuizWithMultiChoicesType ->
                            let msg =
                                match e.Id with
                                | Cyoa.Implementation.NextButtonId ->
                                    Cyoa.Implementation.Next
                                | Cyoa.Implementation.SelectMenuId ->
                                    Cyoa.Implementation.Choice (int e.Values.[0])
                                | x ->
                                    failwithf "expected Id = %s but %s" e.Id x
                            let answer, state = f (Hub.QuizWithMultiChoicesReq msg)
                            let answer =
                                match answer with
                                | Hub.QuizWithMultiChoicesAnswer answer -> answer
                                | x -> failwithf "expected QuizWithMultiChoicesAnswer but %A" x
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
                        | Hub.BallotBoxType ->
                            let answer, state = f Hub.BallotBoxReq
                            let answer =
                                match answer with
                                | Hub.BallotBoxAnswer answer -> answer
                                | x -> failwithf "expected QuizAnswer but %A" x
                            let answer =
                                match answer.View with // TODO: View and ResponseToUser can contain values at the same time
                                | Some x -> Right x
                                | None ->
                                    match answer.ResponseToUser with
                                    | Some x -> Left (Hub.OtherErr x)
                                    | None -> failwith "View and ResponseToUser are None"
                            answer, state

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
                | Hub.SomeGirlsQuizAnswer answer
                | Hub.QuizWithMultiChoicesAnswer answer
                | Hub.QuizAnswer answer -> answer
                | Hub.BallotBoxAnswer answer ->
                    match answer.View with // TODO: View and ResponseToUser can contain values at the same time
                    | Some answer -> answer
                    | None ->
                        match answer.ResponseToUser with
                        | Some answer -> answer
                        | None -> failwith "View and ResponseToUser are None"
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

    match m.PostAndReply(fun r -> Update(msgPath, e, r)) with
    | Right res ->
        let embed = DSharpPlus.Entities.DiscordEmbedBuilder(res.Embed)
        embed.WithAuthor(e.User.Username) |> ignore
        // embed.WithFooter(e.User.Username) |> ignore
        res.Embed <- embed.Build()
        let b = DSharpPlus.Entities.DiscordInteractionResponseBuilder(res)

        e.Interaction.CreateResponseAsync(DSharpPlus.InteractionResponseType.UpdateMessage, b)
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
        | Hub.OtherErr msg ->
            let b = DSharpPlus.Entities.DiscordInteractionResponseBuilder(msg)
            b.IsEphemeral <- true
            e.Interaction.CreateResponseAsync(DSharpPlus.InteractionResponseType.ChannelMessageWithSource, b)
            |> fun x -> x.GetAwaiter().GetResult()
let start appType (client:DSharpPlus.DiscordClient) (e: DSharpPlus.EventArgs.MessageCreateEventArgs) =
    let res = m.PostAndReply(fun r -> Init(e.Author.Id, appType, r))
    let embed = DSharpPlus.Entities.DiscordEmbedBuilder(res.Embed)
    embed.WithAuthor(e.Author.Username) |> ignore
    // embed.WithFooter(e.User.Username) |> ignore
    res.Embed <- embed.Build()

    let msg = await (client.SendMessageAsync (e.Channel, res))

    let msgPath =
        {
            GuildId = msg.Channel.Guild.Id
            ChannelId = msg.Channel.Id
            MessageId = msg.Id
        }
    m.Post(SecondInit(e.Author.Id, appType, msgPath))
