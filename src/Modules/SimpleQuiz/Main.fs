module SimpleQuiz.Main
open DSharpPlus
open FsharpMyExtension
open FsharpMyExtension.Either

open Types
open Extensions
open SimpleQuiz.Model

type FlagImages = WebCacher<System.Drawing.Bitmap>

type State = {
    Players: Players.GuildData
    FlagImages: FlagImages
}

type Request =
    | StartQuiz of quizName: string option
    | StartRating of quizName: string option

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
        >>. opt (many1Satisfy (fun _ -> true))

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

    type QuizType =
        | Capital = 0
        | Flag = 1

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module QuizType =
        let all =
            System.Enum.GetValues(typeof<QuizType>)
            |> Seq.cast<QuizType>
            |> Array.ofSeq

    type Data =
        {
            OwnerId: UserId
            QuizId: Countries.CountryId
            DifficultId: Countries.DifficultId
        }
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Data =
        module Printer =
            open FsharpMyExtension.ShowList

            open Interaction

            let show (data: Data) =
                shows data.OwnerId << nl
                << shows data.QuizId << nl
                << shows data.DifficultId

        module Parser =
            open FParsec

            open Interaction

            let parse: _ ComponentState.Parser.Parser =
                pipe3
                    (puint64 .>> newline)
                    (Countries.CountryId.Parser.parse .>> newline)
                    Countries.DifficultId.Parser.parse
                    (fun id componentId difficultId ->
                        {
                            OwnerId = id
                            QuizId = componentId
                            DifficultId = difficultId
                        }
                    )

    type ComponentState = Interaction.ComponentState<ComponentId, Data>
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module ComponentState =
        let inline serialize (x: ComponentState) =
            Interaction.ComponentState.serialize Data.Printer.show x

        let inline tryDeserialize str: Result<ComponentState, _> option =
            Interaction.ComponentState.tryDeserialize Data.Parser.parse str

    let messageTypeId = "SimpleQuizUI"

    type InitOptions =
        {
            UserId: UserId
            QuizId: Countries.CountryId
            Question: Countries.Country
            QuizType: QuizType
            DifficultId: Countries.DifficultId
        }
        static member create userId quizId question quizType difficultId =
            {
                UserId = userId
                QuizId = quizId
                Question = question
                QuizType = quizType
                DifficultId = difficultId
            }

    let init (options: InitOptions) flagImages (addEmbed, addComponents, addFile) =
        let embed = Entities.DiscordEmbedBuilder()
        embed.Color <- Entities.Optional.FromValue(DiscordEmbed.backgroundColorDarkTheme)

        let componentState =
            let state: ComponentState =
                {
                    Id = messageTypeId
                    ComponentId = ComponentId.AnswerSelectedListId
                    Data = {
                        OwnerId = options.UserId
                        QuizId = options.QuizId
                        DifficultId = options.DifficultId
                    }
                }
            state
            |> ComponentState.serialize

        let description, options, flagImages =
            let varsCount = 8
            let countriesWithoutCurrent =
                let countriesIdsWithoutCurrent =
                    Countries.countries
                    |> Array.filter (fun x -> x <> options.Question.Id)

                Seq.generateRandomSequence countriesIdsWithoutCurrent.Length
                |> Seq.map (fun i ->
                    Countries.countriesById.[countriesIdsWithoutCurrent.[i]]
                )
                |> Seq.truncate (varsCount - 1)
                |> Array.ofSeq

            match options.QuizType with
            | QuizType.Capital ->
                let description = sprintf "<@%d>, какая столица страны \"%s\"?" options.UserId options.Question.Name

                let options =
                    Array.append [|options.Question|] countriesWithoutCurrent // TODO: Array.insertAt
                    |> List.ofArray
                    |> List.shuffle
                    |> List.mapi (fun i item ->
                        Entities.DiscordSelectComponentOption(item.Capital, string item.Id)
                    )

                description, options, None
            | QuizType.Flag ->
                let countries =
                    Array.append [|options.Question|] countriesWithoutCurrent // TODO: Array.insertAt
                    |> List.ofArray
                    |> List.shuffle

                let description =
                    sprintf "<@%d>, какой флаг страны \"%s\"?" options.UserId options.Question.Name

                let flags, flagImages =
                    countries
                    |> List.map (fun x -> x.FlagUrl)
                    |> FlagsRenderer.downloadAndDrawFlags flagImages

                let m = new System.IO.MemoryStream() // potential memory leak
                flags.Save(m, System.Drawing.Imaging.ImageFormat.Png)
                m.Position <- 0L

                let imageUrl = addFile(m)

                embed.ImageUrl <- imageUrl

                let options =
                    countries
                    |> List.mapi (fun i item ->
                        Entities.DiscordSelectComponentOption(string (i + 1), string item.Id)
                    )

                description, options, Some flagImages
            | x ->
                failwithf "%A not implemented yet" x

        embed.Description <- description
        addEmbed (embed.Build())

        let c = Entities.DiscordSelectComponent(componentState, "Выбери ответ...", options)
        addComponents (c :> Entities.DiscordComponent)

        flagImages

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
            match ComponentState.tryDeserialize e.Id with // TODO: test messageTypeId
            | Some res ->
                match res with
                | Ok (componentState: ComponentState) ->
                    match componentState.ComponentId with
                    | ComponentId.AnswerSelectedListId ->
                        let userId = e.User.Id
                        if componentState.Data.OwnerId = userId then
                            match e.Values with
                            | [|answerCountryId|] ->
                                match Countries.CountryId.tryDeserialize answerCountryId with
                                | Ok answerCountryId ->
                                    match Map.tryFind componentState.Data.QuizId Countries.countriesById with
                                    | Some country ->
                                        match Map.tryFind componentState.Data.DifficultId Countries.countriesByDifficult with
                                        | Some difficult ->
                                            let send msg =
                                                let embed = Entities.DiscordEmbedBuilder()
                                                embed.Color <- Entities.Optional.FromValue(DiscordEmbed.backgroundColorDarkTheme)
                                                embed.Description <- msg
                                                let b = Entities.DiscordInteractionResponseBuilder()
                                                b.AddEmbed(embed.Build()) |> ignore
                                                awaiti <| e.Interaction.CreateResponseAsync(InteractionResponseType.UpdateMessage, b)

                                            let ratingPosition scoreType = addWin ((Players.Id.create userId componentState.Data.DifficultId), scoreType)

                                            if country.Id = answerCountryId then
                                                sprintf "<@%d>, и это правильный ответ! 🥳\nТы на %d месте в рейтинге по \"%s\" викторине!"
                                                    userId
                                                    (ratingPosition Win)
                                                    difficult.Name
                                                |> send
                                            else
                                                sprintf "<@%d>, к сожалению, это неправильный ответ.\nТы на %d месте в рейтинге по \"%s\" викторине"
                                                    userId
                                                    (ratingPosition Lose)
                                                    difficult.Name
                                                |> send
                                        | None ->
                                            sprintf "not found in %A question ID in questions" componentState.Data.DifficultId
                                            |> restartComponent
                                    | None ->
                                        sprintf "not found in %A quiz ID in questions" componentState.Data.QuizId
                                        |> restartComponent
                                | Error errorMsg ->
                                    errorMsg
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

    let startQuiz flagImages (addEmbed, addComponents, addFile) userId (difficult: Countries.Difficult) =
        let countries = difficult.Countries
        let countryId = countries.[r.Next(0, countries.Length)]
        let country = Map.find countryId Countries.countriesById

        let quizType =
            let types = QuizUi.QuizType.all
            types.[r.Next(0, types.Length)]

        let initOptions =
            QuizUi.InitOptions.create
                userId
                countryId
                country
                quizType
                difficult.Id

        QuizUi.init initOptions flagImages (addEmbed, addComponents, addFile)

module RatingTableUi =
    open Shared.Ui.Table

    type SortBy =
        | SortByIndex = 0
        | SortByWins = 1
        | SortByLoses = 2

    let initSetting getState: Setting<_, SortBy, Players.QuizId, Players.GuildData> =
        {
            Id = sprintf "RatingTableUi"

            GetState = getState

            Title = fun quizId _ ->
                match Map.tryFind quizId Countries.countriesByDifficult with
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
        (quiz: Countries.Difficult, getState) =

        createTable addComponents addEmbed 1 (None, quiz.Id) (initSetting getState)

    let componentInteractionCreateHandle (client: DiscordClient) (e: EventArgs.ComponentInteractionCreateEventArgs) state =
        componentInteractionCreateHandle client e (initSetting state)

module QuizSelectionUi =
    type ComponentId =
        | QuizSelectionListId = 0

    type Data =
        {
            [<Newtonsoft.Json.JsonProperty("O")>]
            OwnerId: UserId
        }

    type ComponentState = Interaction.ComponentState<ComponentId, Data>

    let init messageTypeId (e: EventArgs.MessageCreateEventArgs) =
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
            Countries.countriesByDifficult
            |> Seq.map (fun (KeyValue(quizId, quiz)) ->
                Entities.DiscordSelectComponentOption(quiz.Name, quizId.ToString())
            )

        let c = Entities.DiscordSelectComponent(componentState, "Выбери викторину...", options)
        b.AddComponents c |> ignore

        awaiti <| e.Channel.SendMessageAsync b

    let handle messageTypeId (client: DiscordClient) (e: EventArgs.ComponentInteractionCreateEventArgs) next =
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
                            | [|rawQuizId|] ->
                                match Countries.DifficultId.tryDeserialize rawQuizId with
                                | Ok quizId ->
                                    match Map.tryFind quizId Countries.countriesByDifficult with
                                    | Some quiz ->
                                        next quiz
                                    | None ->
                                        sprintf "not found in %s quiz ID in quizes" rawQuizId
                                        |> restartComponent
                                | Error errMsg ->
                                    errMsg
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

    module MessageTypesIds =
        let startQuiz = "SimpleQuizSelectionUI"
        let rating = "SimpleQuizSelectionUIRating"

    let handleStartQuiz (client: DiscordClient) (e: EventArgs.ComponentInteractionCreateEventArgs) getSetFlagImages =
        handle MessageTypesIds.startQuiz client e (fun quiz ->
            let b = Entities.DiscordInteractionResponseBuilder()

            let addFile stream =
                let channelForImages =
                    await <| client.GetChannelAsync 930127222373777509UL // todo: remove hard code
                let embed = Entities.DiscordMessageBuilder()
                let fileName = "flags.png"
                embed.WithFile(fileName, stream) |> ignore
                let x =
                    await <| client.SendMessageAsync(channelForImages, embed)

                stream.Dispose()
                x.Attachments.[0].Url

            getSetFlagImages (fun flagImages ->
                Actions.startQuiz
                    flagImages
                    (b.AddEmbed >> ignore, b.AddComponents >> ignore, addFile)
                    e.User.Id
                    quiz
            )

            awaiti <| e.Interaction.CreateResponseAsync(InteractionResponseType.UpdateMessage, b)
        )

    let handleRating (client: DiscordClient) (e: EventArgs.ComponentInteractionCreateEventArgs) getPlayers =
        handle MessageTypesIds.rating client e (fun quiz ->
            let b = Entities.DiscordInteractionResponseBuilder()

            RatingTableUi.createTable
                (b.AddEmbed >> ignore, b.AddComponents >> ignore)
                (quiz, getPlayers)

            awaiti <| e.Interaction.CreateResponseAsync(InteractionResponseType.UpdateMessage, b)
        )

type Msg =
    | Request of EventArgs.MessageCreateEventArgs * Request
    /// returned player position in rating
    | AddScore of AsyncReplyChannel<int> * (Players.Id * ScoreType)
    | GetPlayers of AsyncReplyChannel<Players.GuildData>
    | GetFlagImages of AsyncReplyChannel<FlagImages>
    | SetFlagImages of FlagImages

let actionReduce (e: EventArgs.MessageCreateEventArgs) (msg: Request) (state: State) =
    let tryFindQuizByName quizName =
        Countries.countriesByDifficult
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
            QuizSelectionUi.init QuizSelectionUi.MessageTypesIds.startQuiz e

            state
        | Some quizName ->
            match tryFindQuizByName quizName with
            | Some quiz ->
                let b = Entities.DiscordMessageBuilder()
                let addEmbed c = b.Embed <- c
                let addFile stream =
                    let fileName = "flags.png"
                    b.WithFile(fileName, stream) |> ignore
                    sprintf "attachment://%s" fileName

                let flagImages =
                    Actions.startQuiz
                        state.FlagImages
                        (addEmbed, b.AddComponents >> ignore, addFile)
                        e.Author.Id
                        quiz

                awaiti <| e.Channel.SendMessageAsync b

                match flagImages with
                | None -> state
                | Some flagImages ->
                    { state with
                        FlagImages = flagImages
                    }

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

        match quizName with
        | None ->
            QuizSelectionUi.init QuizSelectionUi.MessageTypesIds.rating e
        | Some quizName ->
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

    | GetFlagImages r ->
        r.Reply state.FlagImages

        state

    | SetFlagImages flagImages ->
        { state with
            FlagImages = flagImages
        }

let m =
    let init: State = {
        Players = Players.GuildData.init Db.database
        FlagImages =
            let isLoadFlagsOnStartup = true

            if isLoadFlagsOnStartup then
                printfn "Downloading flags..."
                let urls =
                    Countries.countriesById
                    |> Seq.map (fun (KeyValue(k, v)) -> v.FlagUrl)
                let _, webCacher = FlagsRenderer.downloads urls WebCacher.empty
                printfn "Flags has been downloaded."
                webCacher
            else
                WebCacher.empty
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
    || QuizSelectionUi.handleStartQuiz client e (fun getSetflagImages ->
        let flagImages =
            m.PostAndReply GetFlagImages
            |> getSetflagImages

        match flagImages with
        | Some flagImages ->
            m.Post (SetFlagImages flagImages)
        | None -> ()
    )
    || RatingTableUi.componentInteractionCreateHandle client e (fun () ->
        m.PostAndReply GetPlayers
    )
    || QuizSelectionUi.handleRating client e (fun () ->
        m.PostAndReply GetPlayers
    )
