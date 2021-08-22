module Cyoa

module Core =
    type Var =
        | String of string
        | Bool of bool
        | Num of int
    type Vars = Map<string, Var>

    type Stmt<'LabelName, 'Addon> =
        | Say of DSharpPlus.Entities.DiscordEmbed
        | Jump of 'LabelName
        | Menu of DSharpPlus.Entities.DiscordEmbed * (string * Stmt<'LabelName, 'Addon> list) list
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
        say' txt
        |> Say

    let says (xs:string list) =
        let b = DSharpPlus.Entities.DiscordEmbedBuilder()
        b.Description <-
            xs
            |> String.concat "\n"
        b.Build()

    let jump (labelName:'LabelName) =
        Jump labelName

    let choice (caption:string) (body:Stmt<'LabelName, 'Addon> list) = caption, body
    let menu caption xs = Menu(caption, xs)
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
            | Say x ->
                Print(x, fun () ->
                    next id stack
                )
            | Menu(caption, xs) ->
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
                        Say (getScreen 4)
                    ]
                    choice "Зайти во дворец" [
                        Say (getScreen 5)
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
                        Say (getScreen 7)
                    ]
                ]
            ]
        ]

    open FsharpMyExtension.ListZipper
    let beginLoc = Label1
    let start () =
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
        {|
            Scenario = scenario
            Init = init
        |}
    let interp gameState =
        let x = start ()
        gameState
        |> interp
            (fun next state isWin addon ->
                failwith "addon not implemented"
            )
            x.Scenario

open Types
open Implementation

let scenario = Scenario.start ()
let initState : State<Scenario.LabelName,obj,obj> =
    {
        Game =
            Scenario.interp scenario.Init
        GameState = scenario.Init
        SavedGameState = scenario.Init
    }
type States = Map<Types.UserId, MessageId option * State<Scenario.LabelName,obj,obj>>

type Request =
    | InitGame
    | SetGameMessageId of MessageId
    | ContinueGame of gameMsgId:MessageId * Implementation.Msg

type Answer =
    /// Если игра уже существовала, то возвращает старое сообщение, кнопки которого нужно убрать, чтобы ложно не срабатывали
    | InitGameAnswer of DSharpPlus.Entities.DiscordMessageBuilder * MessageId option
    /// Игра создана с таким-то сообщением, но запрос был с другим
    | GameNotFound of oldMessage:MessageId
    | GameNotStartedYet
    | Ok of DSharpPlus.Entities.DiscordMessageBuilder
    | SetGameMessageIdDone
    | GameMessageIdNotSetYet

let m =
    let exec (st:States) ((userId, msg:Request), rep:AsyncReplyChannel<Answer>) : States =
        match msg with
        | ContinueGame (gameMsgId, msg) ->
            match Map.tryFind userId st with
            | Some (gameMsgId', gameState) ->
                match gameMsgId' with
                | Some gameMsgId' ->
                    if gameMsgId = gameMsgId' then
                        let gameState = update Scenario.interp scenario.Init msg gameState
                        gameView (fun _ _ -> failwith "not imp") gameState
                        |> Ok
                        |> rep.Reply

                        match gameState.Game with
                        | Core.End ->
                            Map.remove userId st
                        | _ ->
                            Map.add userId (Some gameMsgId', gameState) st
                    else
                        rep.Reply (GameNotFound gameMsgId')

                        st
                | None ->
                    rep.Reply GameMessageIdNotSetYet

                    st
            | None ->
                rep.Reply GameNotStartedYet
                st
        | InitGame ->
            match Map.tryFind userId st with
            | Some (oldGameMsgId, _) ->
                let gameState = update Scenario.interp scenario.Init NewGame initState
                gameView (fun _ _ -> failwith "not imp") gameState
                |> fun res -> InitGameAnswer(res, oldGameMsgId)
                |> rep.Reply

                Map.add userId (None, gameState) st
            | None ->
                let gameState = update Scenario.interp scenario.Init NewGame initState
                gameView (fun _ _ -> failwith "not imp") gameState
                |> fun res -> InitGameAnswer(res, None)
                |> rep.Reply

                Map.add userId (None, gameState) st
        | SetGameMessageId gameMsgId ->
            match Map.tryFind userId st with
            | Some (_, gameState) ->
                rep.Reply SetGameMessageIdDone

                Map.add userId (Some gameMsgId, gameState) st
            | None ->
                rep.Reply GameNotStartedYet

                st

    MailboxProcessor.Start (fun mail ->
        let rec loop (st:States) =
            async {
                let! res = mail.Receive()
                let st =
                    try
                        exec st res
                    with err ->
                        printfn "%A" err
                        st

                return! loop st
            }
        loop Map.empty
    )


let removeComponents (client:DSharpPlus.DiscordClient) channelId (messageId:MessageId) =
    let x = await (client.GetChannelAsync(channelId))
    try
        let oldMsg =
            await (x.GetMessageAsync messageId)

        let b = DSharpPlus.Entities.DiscordMessageBuilder()
        // embed, по-идеи, остается. Здесь главное подавить .Components
        b.Content <- oldMsg.Content

        awaiti (oldMsg.ModifyAsync(b))
    with e -> ()

let resp (client:DSharpPlus.DiscordClient) (e: DSharpPlus.EventArgs.ComponentInteractionCreateEventArgs) =
    let msg =
        match e.Id with
        | NextButtonId ->
            Next
        | SelectMenuId ->
            Choice (int e.Values.[0])
        | x ->
            failwithf "expected Id = %s but %s" e.Id x

    match m.PostAndReply(fun r -> (client.CurrentUser.Id, ContinueGame (e.Message.Id, msg)), r) with
    | Ok res ->
        res
        |> e.Message.ModifyAsync
        |> awaiti

        e.Interaction.CreateResponseAsync(DSharpPlus.InteractionResponseType.UpdateMessage)
        |> fun x -> x.GetAwaiter().GetResult()
    | InitGameAnswer(res, oldMsgId) ->
        match oldMsgId with
        | None -> ()
        | Some oldMsgId ->
            let channelId = // TODO: сообщение может быть из другого канала, а то и вовсе из другой гильдии
                e.Channel.Id
            removeComponents client channelId oldMsgId

        res
        |> e.Message.ModifyAsync
        |> awaiti

        e.Interaction.CreateResponseAsync(DSharpPlus.InteractionResponseType.UpdateMessage)
        |> fun x -> x.GetAwaiter().GetResult()
    | SetGameMessageIdDone -> ()
    | GameNotStartedYet ->
        let b = DSharpPlus.Entities.DiscordInteractionResponseBuilder()
        b.Content <- "Бот перезагружался, и игра слетела. Начните заново."
        b.IsEphemeral <- true

        e.Interaction.CreateResponseAsync(DSharpPlus.InteractionResponseType.ChannelMessageWithSource, b)
        |> fun x -> x.GetAwaiter().GetResult()
    | GameNotFound(oldMessage) ->
        let b = DSharpPlus.Entities.DiscordInteractionResponseBuilder()
        b.Content <- "Скорее всего, это чужая игра. Ваша игра [тут](), или начните новую." // TODO: ссылка на игру
        b.IsEphemeral <- true

        e.Interaction.CreateResponseAsync(DSharpPlus.InteractionResponseType.ChannelMessageWithSource, b)
        |> fun x -> x.GetAwaiter().GetResult()
    | GameMessageIdNotSetYet -> failwith "GameMessageIdNotSetYet"

let start (client:DSharpPlus.DiscordClient) (e: DSharpPlus.EventArgs.MessageCreateEventArgs) =
    match m.PostAndReply(fun r -> (client.CurrentUser.Id, InitGame), r) with
    | InitGameAnswer(res, d) ->
        match d with
        | None -> ()
        | Some oldMsgId ->
            let channelId = // TODO: сообщение может быть из другого канала, а то и вовсе из другой гильдии
                e.Channel.Id
            removeComponents client channelId oldMsgId

        let msg = await (client.SendMessageAsync (e.Channel, res))
        match m.PostAndReply(fun r -> (client.CurrentUser.Id, SetGameMessageId msg.Id), r) with
        | SetGameMessageIdDone -> ()
        | x ->
            failwithf "expected SetGameMessageIdDone but %A" x
    | x -> failwithf "expected InitGameAnswer but %A" x
