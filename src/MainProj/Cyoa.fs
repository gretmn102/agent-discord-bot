module Cyoa

module Core =
    type Var =
        | String of string
        | Bool of bool
        | Num of int
        | BoolArr of bool []
    type Vars = Map<string, Var>

    type Stmt<'LabelName, 'Addon> =
        | Say of (Vars -> DSharpPlus.Entities.DiscordEmbed)
        | Jump of 'LabelName
        | Menu of (Vars -> DSharpPlus.Entities.DiscordEmbed * (string * Stmt<'LabelName, 'Addon> list) list)
        | If of (Vars -> bool) * Stmt<'LabelName, 'Addon> list * Stmt<'LabelName, 'Addon> list
        | ChangeVars of (Vars -> Vars)
        | Addon of 'Addon

    type Label<'LabelName, 'Addon> = 'LabelName * Stmt<'LabelName, 'Addon> list
    let label (labelName:'LabelName) (stmts:Stmt<_, _> list) =
        labelName, stmts
        : Label<_,_>

    let say' (txt:string) =
        let b = DSharpPlus.Entities.DiscordEmbedBuilder()
        b.Description <- txt
        b.Color <- DSharpPlus.Entities.Optional.FromValue(DSharpPlus.Entities.DiscordColor("#2f3136"))

        b.Build()
    let say (txt:string) =
        Say (fun _ -> say' txt)

    let says (xs:string list) =
        let b = DSharpPlus.Entities.DiscordEmbedBuilder()
        b.Description <-
            xs
            |> String.concat "\n"
        b.Build()

    let jump (labelName:'LabelName) =
        Jump labelName

    let choice (caption:string) (body:Stmt<'LabelName, 'Addon> list) = caption, body

    let menu caption xs = Menu(fun _ -> caption, xs)
    let menuV caption xs = Menu (fun vars -> caption vars, xs vars)

    let if' pred thenBody elseBody =
        If(pred, thenBody, elseBody)
    type Scenario<'LabelName, 'Addon> when 'LabelName : comparison =
        Map<'LabelName, Label<'LabelName, 'Addon>>


    open FsharpMyExtension.ListZipper

    type State<'LabelName, 'Addon> =
        {
            LabelState: ListZ<Stmt<'LabelName, 'Addon>> list
            Vars: Vars
        }

    type T<'LabelName, 'Addon, 'Arg> =
        | Print of DSharpPlus.Entities.DiscordEmbed * (unit -> T<'LabelName, 'Addon, 'Arg>)
        | PrintEnd of DSharpPlus.Entities.DiscordEmbed
        | Choices of DSharpPlus.Entities.DiscordEmbed * string list * (int -> T<'LabelName, 'Addon, 'Arg>)
        | End
        | AddonAct of 'Addon * ('Arg -> T<'LabelName, 'Addon, 'Arg>)
        | NextState of State<'LabelName, 'Addon>

    let interp addon (scenario: Scenario<'LabelName,'Addon>) (state:State<'LabelName, 'Addon>) =
        let next changeState stack =
            let rec next = function
                | x::xs ->
                    match ListZ.next x with
                    | Some x ->
                        { state with
                            LabelState = x::xs }
                        |> Some
                    | None -> next xs
                | [] -> None
            match next stack with
            | Some state ->
                NextState (changeState state)
            | None -> End
        match state.LabelState with
        | headStack::tailStack as stack ->
            let x = ListZ.hole headStack
            match x with
            | Jump x ->
                { state with
                    LabelState =
                        match snd scenario.[x] with
                        | [] -> []
                        | xs ->
                            [ ListZ.ofList xs ] }
                |> NextState
            | Say f ->
                match next id stack with
                | End ->
                    PrintEnd (f state.Vars)
                | nextStmt ->
                    Print(f state.Vars, fun () -> nextStmt)
            | Menu f ->
                let caption, xs = f state.Vars
                let labels = xs |> List.map fst
                Choices(caption, labels, fun i ->
                    let _, body = xs.[i]

                    if List.isEmpty body then
                        next id stack
                    else
                        { state with
                            LabelState =
                                ListZ.ofList body::stack }
                        |> NextState
                )
            | If(pred, thenBody, elseBody) ->
                let f body =
                    if List.isEmpty body then
                        next id stack
                    else
                        { state with
                            LabelState =
                                ListZ.ofList body::stack }
                        |> NextState
                if pred state.Vars then
                    f thenBody
                else
                    f elseBody
            | Addon addonArg ->
                AddonAct(addonArg, fun res ->
                    addon next state res addonArg
                )
            | ChangeVars f ->
                stack
                |> next (fun state -> { state with Vars = f state.Vars })
        | [] -> End

module Implementation =
    open FsharpMyExtension.Either

    type State<'LabelName, 'Addon, 'Arg> =
        {
            Game: Core.T<'LabelName, 'Addon, 'Arg>
            GameState: Core.State<'LabelName, 'Addon>

            SavedGameState: Core.State<'LabelName, 'Addon>
        }

    type Msg =
        | Next
        | Choice of int
        | NextState
        | Save
        | Load
        | NewGame

    let update interp scenarioInit (msg: Msg) (state: State<'LabelName, 'Addon, 'Arg>) =
        let nextState x =
            let rec nextState gameState = function
                | Core.NextState newGameState ->
                    nextState newGameState (interp newGameState)
                | game ->
                    { state with
                        GameState = gameState
                        Game = game }
            nextState state.GameState x
        match msg with
        | Next ->
            match state.Game with
            | Core.Print(_, f) ->
                nextState (f ())
            | Core.NextState x ->
                failwith "nextNextState"
            | Core.PrintEnd _
            | Core.End
            | Core.Choices _
            | Core.AddonAct _ ->
                state
        | Choice i ->
            match state.Game with
            | Core.Choices(_, _, f)->
                nextState (f i)
            | Core.Print(_, f) ->
                nextState (f ())
            | Core.NextState x ->
                failwith "choiceNextState"
            | Core.PrintEnd _
            | Core.End
            | Core.AddonAct _ -> state
        | NextState ->
            nextState state.Game
        | Save ->
            let state =
                { state with
                    SavedGameState = state.GameState }
            state
        | Load ->
            let state =
                let gameState = state.SavedGameState
                { state with
                    Game = interp gameState
                    GameState = gameState }
            state
        | NewGame ->
            let state =
                { state with
                    Game = interp scenarioInit }
            state

    [<Literal>]
    let NextButtonId = "nextButtonId"
    [<Literal>]
    let SelectMenuId = "selectMenuId"

    let gameView addon (state:State<'LabelName, 'Addon, 'Arg>) =
        match state.Game with
        | Core.Print(embed, _) ->
            let b = DSharpPlus.Entities.DiscordMessageBuilder()

            let nextButton = DSharpPlus.Entities.DiscordButtonComponent(DSharpPlus.ButtonStyle.Primary, NextButtonId, "...")

            b.Embed <- embed

            b.AddComponents nextButton |> ignore
            b
        | Core.PrintEnd embed ->
            let b = DSharpPlus.Entities.DiscordMessageBuilder()
            b.Embed <- embed
            b
        | Core.End ->
            let b = DSharpPlus.Entities.DiscordMessageBuilder()
            let embed = DSharpPlus.Entities.DiscordEmbedBuilder()
            embed.Description <- "Конец"
            b.Embed <- embed.Build()

            b
        | Core.Choices(caption, choices, _) ->
            let b = DSharpPlus.Entities.DiscordMessageBuilder()
            b.Embed <- caption

            let options =
                choices
                |> List.mapi (fun i label ->
                    DSharpPlus.Entities.DiscordSelectComponentOption(label, string i)
                )

            let c = DSharpPlus.Entities.DiscordSelectComponent(SelectMenuId, "select", options)
            b.AddComponents c |> ignore

            b
        | Core.AddonAct(arg, _) ->
            addon arg state
        | Core.NextState x -> failwithf "NextState %A" x

module Scenario =
    open Core

    type LabelName =
        | Label1
        | Label2
        | Label3

    let mushroom = "mushroom"
    let getmushroom (x:Map<_,_>) =
        match x.[mushroom] with
        | Bool x -> x
        | _ -> failwith ""
    let counter = "counter"
    let getCounter (x:Map<_,_>) =
        match x.[counter] with
        | Num x -> x
        | _ -> failwith ""
    let counterSet fn =
        ChangeVars (fun vars ->
            match Map.tryFind counter vars with
            | Some (Num x) ->
                Map.add counter (Num (fn x)) vars
            | _ -> Map.add counter (Num (fn 0)) vars
        )
    let scenario : list<Label<LabelName,obj>> =
        let img src =
            let b = DSharpPlus.Entities.DiscordEmbedBuilder()
            b.ImageUrl <- src
            b.Color <- DSharpPlus.Entities.Optional.FromValue(DSharpPlus.Entities.DiscordColor("#2f3136"))
            b.Build()

        // https://imgur.com/a/FXkyeUC
        let images =
            [|
                "https://i.imgur.com/cwyAD9v.jpg"
                "https://i.imgur.com/q6va92F.jpg"
                "https://i.imgur.com/jA5FFtS.jpg"
                "https://i.imgur.com/UCJh9S8.jpg"
                "https://i.imgur.com/vW1k32H.jpg"
                "https://i.imgur.com/M8kApsD.jpg"
                "https://i.imgur.com/uOPC36L.jpg"
            |]
        let getScreen i = img images.[i - 1]
        let say x =
            Say (fun _ -> x)
        [
            label Label1 [
                menu (getScreen 1) [
                    choice "Войти" [
                        jump Label2
                    ]
                    choice "Остаться на месте" [
                        jump Label3
                    ]
                ]
            ]
            label Label2 [
                menu (getScreen 2) [
                    choice "Вернуться" [
                        jump Label1
                    ]
                    choice "Исследовать местность" [
                        say (getScreen 4)
                    ]
                    choice "Зайти во дворец" [
                        say (getScreen 5)
                    ]
                ]
            ]
            label Label3 [
                menu (getScreen 3) [
                    choice "Передумать" [
                        jump Label1
                    ]
                    choice "Исследовать Лес Невозврата" [
                        menu (getScreen 6) [
                            choice "Войти" [
                                jump Label2
                            ]
                            choice "Остаться на месте" [
                                jump Label3
                            ]
                        ]
                    ]
                    choice "Войти во Дворец Зла" [
                        say (getScreen 7)
                    ]
                ]
            ]
        ]

    let beginLoc = Label1

module Scenario2 =
    open Core

    type LabelName =
        | Prelude
        | StartQuiz
        | Result

    let counter = "counter"
    let getCounter (x:Map<_,_>) =
        match x.[counter] with
        | Num x -> x
        | _ -> failwith ""
    let counterSet fn =
        ChangeVars (fun vars ->
            match Map.tryFind counter vars with
            | Some (Num x) ->
                Map.add counter (Num (fn x)) vars
            | _ -> Map.add counter (Num (fn 0)) vars
        )

    let scenario prelude quizPath : list<Label<LabelName,obj>> =
        let questions, results = Quiz.loadQuiz quizPath
        [
            prelude

            label StartQuiz [
                for x in questions do
                    menu (say' x.Description) [
                        for description, score in x.Variants do
                            choice description [
                                counterSet ((+) score)
                            ]
                    ]
                jump Result
            ]

            label Result [
                Say (fun vars ->
                    let score = getCounter vars
                    let res =
                        results
                        |> List.tryFind (fun x ->
                            let from, to' = x.Range
                            from <= score && score <= to')
                    match res with
                    | Some x ->
                        let b = DSharpPlus.Entities.DiscordEmbedBuilder()
                        b.Description <- sprintf "%d очков\n\n%s" score x.Description
                        b.Color <- DSharpPlus.Entities.Optional.FromValue(DSharpPlus.Entities.DiscordColor("#2f3136"))
                        match x.ImgSrc with
                        | Some imgSrc ->
                            b.WithImageUrl(imgSrc) |> ignore
                        | None -> ()

                        b.Build()
                    | None -> failwith "Something wrong"
                )
            ]
        ]

    let someGirlsQuiz =
        let prelude =
            label Prelude [
                menu (say' "Какой-то тест для девушек, украденный с [этого видео на YouTube](https://www.youtube.com/watch?v=kuAKQ-qOqrU).") [
                    choice "Поехали!" [
                        jump StartQuiz
                    ]
                    choice "Да ну нафиг!" [
                        say "Как хочешь ¯\\_(ツ)_/¯"
                    ]
                ]
            ]
        scenario prelude "Quiz.json"

    let quizPizza =
        let prelude =
            label Prelude [
                menu (say' "Тест: Какая ты пицца?\n\nТест беспардонно взят с [этого сайта](https://kaktutzhit.by/test/dominos).") [
                    choice "Поехали!" [
                        jump StartQuiz
                    ]
                    choice "Да ну нафиг!" [
                        say "Как хочешь ¯\\_(ツ)_/¯"
                    ]
                ]
            ]

        scenario prelude "QuizPizza.json"

    let beginLoc = Prelude

module QuizWithMultipleChoice =
    open Core

    type LabelName =
        | Prelude
        | StartQuiz of int
        | Loop of int
        | Result

    let counter = "counter"
    let getCounter (x:Map<_,_>) =
        match x.[counter] with
        | Num x -> x
        | _ -> failwith ""
    let setCounter fn =
        ChangeVars (fun vars ->
            match Map.tryFind counter vars with
            | Some (Num x) ->
                Map.add counter (Num (fn x)) vars
            | _ -> Map.add counter (Num (fn 0)) vars
        )

    let bools = "bools"
    let getBools (x:Map<_,_>) =
        match x.[bools] with
        | BoolArr x -> x
        | _ -> failwith ""
    let setBools fn =
        ChangeVars (fun vars ->
            match Map.tryFind bools vars with
            | Some (BoolArr xs) ->
                Map.add bools (BoolArr (fn xs)) vars
            | _ -> Map.add bools (BoolArr (fn [||])) vars
        )
    type T =
        {
            Choice: (int -> T)
            Pass: int
        }
    type State =
        { Choices: (int * bool) [] }
    let rec f (st:State) =
        {
            Choice = fun i ->
                { st with
                    Choices =
                        let xs = st.Choices
                        let v, _ = xs.[i]
                        xs.[i] <- v, true
                        xs
                }
                |> f
            Pass =
                st.Choices |> Array.sumBy fst
        }

    let scenario : list<Label<LabelName,obj>> =
        [
            label Prelude [
                menu (say' "Какая ты пицца сегодня?") [
                    choice "Поехали!" [
                        jump (StartQuiz 0)
                    ]
                    choice "Да ну нафиг!" [
                        say "Как хочешь ¯\\_(ツ)_/¯"
                    ]
                ]
            ]

            let f questIndex (choices: _ []) next =
                [
                    label (StartQuiz questIndex) [
                        setBools (fun _ -> Array.create choices.Length false)

                        jump (Loop questIndex)
                    ]
                    label (Loop questIndex) [
                        menuV
                            (fun vars ->
                                "Выберите несколько пунктов и нажмите \"Дальше\""
                                |> say'
                            )
                            (fun vars ->
                                [
                                    let bools = getBools vars
                                    for i, (description, score) in Array.indexed choices do
                                        let f i =
                                            if bools.[i] then
                                                sprintf "● %s" description
                                            else
                                                sprintf "○ %s" description
                                        choice (f i) [
                                            setBools (fun xs ->
                                                xs.[i] <- not xs.[i]
                                                xs
                                            )
                                            jump (Loop questIndex)
                                        ]
                                    if Array.exists id bools then
                                        choice "Дальше" [
                                            setCounter (fun counter ->
                                                bools
                                                |> Array.fold
                                                    (fun (i, counter) hasSelected ->
                                                        let counter =
                                                            if hasSelected then
                                                                counter + snd choices.[i]
                                                            else
                                                                counter
                                                        i + 1, counter
                                                    )
                                                    (0, counter)
                                                |> snd
                                            )
                                            jump next
                                        ]
                                ]
                            )
                    ]
                ]
            let choices =
                [|
                    "Один", 1
                    "Два", 2
                    "Три", 3
                |]
            yield! f 0 choices (StartQuiz 1)
            let choices =
                [|
                    "Один1", 1
                    "Два2", 2
                    "Три3", 3
                    "Четыре4", 4
                |]
            yield! f 1 choices Result

            label Result [
                Say (fun vars ->
                    let score = getCounter vars
                    say' (sprintf "%d" score)
                )
            ]
        ]

    let beginLoc = Prelude

open Core
open Implementation
open FsharpMyExtension.ListZipper

type All<'LabelName, 'Addon, 'Arg when 'LabelName : comparison> =
    {
        Init: State<'LabelName,'Addon>

        IfState: State<'LabelName,'Addon,'Arg>
        Interp: State<'LabelName,'Addon> -> T<'LabelName,'Addon,'Arg>
    }

let initState beginLoc scenario =
    let scenario =
        scenario
        |> List.map (fun (labelName, body) -> labelName, (labelName, body))
        |> Map.ofList
        : Scenario<_,_>
    let init =
        {
            LabelState =
                [ ListZ.ofList (snd scenario.[beginLoc]) ]
            Vars = Map.empty
        }

    let interp gameState =
        gameState
        |> interp
            (fun next state isWin addon ->
                failwith "addon not implemented"
            )
            scenario
    {
        Init = init

        Interp = interp
        IfState =
            {
                Game = interp init
                GameState = init
                SavedGameState = init
            }
    }