module Fishing.Main
open DSharpPlus
open FsharpMyExtension

open Types
open Extensions
open Model

type State =
    {
        Players: Players.GuildData
    }

type ActionReq =
    | ToFish of bait: ItemId option
    | Inventory
    | Progress

type Request =
    | ActionReq of ActionReq

module Parser =
    open FParsec

    open DiscordMessage.Parser

    type 'Result Parser = Primitives.Parser<'Result, unit>

    module CommandNames =
        let toFish = "рыбачить"
        let inventory = "инвентарь"
        let progress = "прогресс"

    let paction: _ Parser =
        let toFish =
            skipStringCI CommandNames.toFish .>> spaces
            >>. opt (many1Satisfy (fun _ -> true))

        choice [
            toFish |>> ToFish
            skipStringCI CommandNames.inventory >>% Inventory
            skipStringCI CommandNames.progress >>% Progress
        ]

    let start: _ Parser =
        choice [
            paction |>> Request.ActionReq
        ]

type Msg =
    | Request of EventArgs.MessageCreateEventArgs * Request
    | ToFishHandle of EventArgs.ComponentInteractionCreateEventArgs * bait: ItemId option
    | GetState of AsyncReplyChannel<State>

module InventoryTable =
    open Shared.Ui.Table

    type SortBy =
        | SortByName = 0
        | SortByCount = 1

    let initSetting (state: Inventory): Setting<_, SortBy> =
        {
            Id = "InventoryTable"

            Title = "Инвентарь"

            GetHeaders = fun sortBy ->
                match sortBy with
                | SortBy.SortByName ->
                    [| "Название▼"; "Кол-во" |]
                | SortBy.SortByCount ->
                    [| "Учасники"; "Кол-во▼" |]
                | x ->
                    failwithf "InventoryTable.SortBy %A" x

            GetItems = fun () ->
                state
                |> Map.toArray

            ItemsCountPerPage = 10

            SortBy = SortByContainer.Init [|
                SortBy.SortByName, "Отсортировать по названию"
                SortBy.SortByCount, "Отсортировать по количеству"
            |]

            SortFunction = fun sortBy items ->
                match sortBy with
                | SortBy.SortByName -> items
                | SortBy.SortByCount ->
                    Array.sortByDescending (fun (_, data) -> data.Count) items
                | x ->
                    failwithf "MostActiveTable.SortBy %A" x

            MapFunction =
                fun i (userId, user) ->
                    [|
                        userId
                        string user.Count
                    |]
        }

    let createTable
        (addComponents: Entities.DiscordComponent [] -> _)
        addEmbed
        userRanks =

        createTable addComponents addEmbed 1 None (initSetting userRanks)

    let componentInteractionCreateHandle getState (client: DiscordClient) (e: EventArgs.ComponentInteractionCreateEventArgs) =
        let state: State = getState ()

        let userRanks =
            match Players.GuildData.tryFind e.User.Id state.Players with
            | Some ranking -> ranking.Inventory
            | None -> Map.empty

        componentInteractionCreateHandle client e (initSetting userRanks)

let createProgressMessage totalCatchesCount =
    sprintf "Найдено %d обитателей морских глубин из %d."
        totalCatchesCount
        items.Count

module BaitChoiceUi =
    type ComponentId =
        | BaitChoiceId = 0

    type Data =
        {
            OwnerId: UserId
        }

    type BaitComponentState = Interaction.ComponentState<ComponentId, Data>

    let messageTypeId = "BaitChoiceComponentId"

    let init (baits: Item list) (e: EventArgs.MessageCreateEventArgs) =
        let userId = e.Author.Id

        let embed = Entities.DiscordEmbedBuilder()
        embed.Color <- Entities.Optional.FromValue(DiscordEmbed.backgroundColorDarkTheme)
        embed.Description <- sprintf "<@%d>, выбери наживку:" userId

        let b = DSharpPlus.Entities.DiscordMessageBuilder()
        b.Embed <- embed.Build()

        let options =
            withoutBait::baits
            |> List.mapi (fun i item ->
                DSharpPlus.Entities.DiscordSelectComponentOption(item.Name, item.ItemId)
            )

        let componentState =
            let state: BaitComponentState =
                {
                    Id = messageTypeId
                    ComponentId = ComponentId.BaitChoiceId
                    Data = {
                        OwnerId = userId
                    }
                }
            state
            |> Interaction.ComponentState.Serialize

        let c = Entities.DiscordSelectComponent(componentState, "Выбери наживку...", options)
        b.AddComponents c |> ignore

        awaiti <| e.Channel.SendMessageAsync b

    let handle (client: DiscordClient) (e: EventArgs.ComponentInteractionCreateEventArgs) next =
        let restartComponent errMsg =
            DiscordMessage.Ext.clearComponents e.Message

            let b = Entities.DiscordInteractionResponseBuilder()
            b.Content <-
                [
                    sprintf "Вызовите комманду `.%s` еще раз, потому что-то пошло не так:" Parser.CommandNames.toFish
                    "```"
                    sprintf "%s" errMsg
                    "```"
                ] |> String.concat "\n"
            b.IsEphemeral <- true
            awaiti <| e.Interaction.CreateResponseAsync(InteractionResponseType.ChannelMessageWithSource, b)

        if e.Message.Author.Id = client.CurrentUser.Id then
            match Interaction.ComponentState.TryDeserialize messageTypeId e.Id with
            | Some res ->
                match res with
                | Ok (data: BaitComponentState) ->
                    match data.ComponentId with
                    | ComponentId.BaitChoiceId ->
                        if data.Data.OwnerId = e.User.Id then
                            let selected = e.Values.[0]

                            let selectedBait =
                                if selected = withoutBait.ItemId then
                                    None
                                else
                                    Some selected

                            next selectedBait
                        else
                            let b = Entities.DiscordInteractionResponseBuilder()
                            b.Content <-
                                sprintf "Здесь рыбачит <@%d>. Начните рыбачить сами командой `.%s`"
                                    data.Data.OwnerId
                                    Parser.CommandNames.toFish
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

let r = new System.Random()

module Actions =
    let toFish baitName send (userId: UserId) (state: State) =
        let sendCatched (newCatchSetting: option<{| TotalCatchesCount: int |}>) (catch: Item) =
            [
                sprintf "Поймано \"%s\"!" catch.Name
                match newCatchSetting with
                | Some newCatchSetting ->
                    createProgressMessage newCatchSetting.TotalCatchesCount
                | None -> ()
            ]
            |> String.concat "\n"
            |> send

        let useBaseBaitOrContinue next =
            match baitName with
            | Some baitName ->
                next baitName
            | None ->
                let player =
                    match Players.GuildData.tryFind userId state.Players with
                    | Some player -> player
                    | None -> Players.MainData.Empty

                let inventory: Inventory =
                    player.Inventory
                    |> Inventory.update baseItemId ((+) 1)

                let state =
                    {
                        Players =
                            state.Players
                            |> Players.GuildData.set
                                userId
                                (fun p -> { p with Inventory = inventory })
                    }

                sendCatched None items.[baseItemId]

                state

        let getBait baitName next =
            match Map.tryFind baitName items with
            | Some bait ->
                if Array.isEmpty bait.Loot then
                    let msg = sprintf "На \"%s\" больше ничего словить нельзя." baitName
                    send msg

                    state
                else
                    next bait

            | None ->
                let msg = sprintf "Предмет \"%s\" в игре не найден." baitName
                send msg

                state

        let sendThereIsNoMoreBait (bait: Item) =
            let msg = sprintf "Наживок \"%s\" в инвентаре больше нет." bait.Name
            send msg

            state

        let getCurrentPlayer bait next =
            match Players.GuildData.tryFind userId state.Players with
            | Some player ->
                next player
            | None ->
                sendThereIsNoMoreBait bait

        let getBaitFromInventory (bait: Item) (player: Players.MainData) next =
            match Map.tryFind bait.ItemId player.Inventory with
            | Some baitInInventory ->
                if baitInInventory.Count > 0 then
                    next baitInInventory
                else
                    sendThereIsNoMoreBait bait
            | None ->
                sendThereIsNoMoreBait bait

        useBaseBaitOrContinue <| fun baitName ->
        getBait baitName <| fun bait ->
        getCurrentPlayer bait <| fun player ->
        getBaitFromInventory bait player <| fun baitInInventory ->
        // remove bait from inventory
        let inventory: Inventory =
            player.Inventory
            |> Inventory.update bait.ItemId (fun count -> count - 1)

        let loot = bait.Loot
        let catchId = loot.[r.Next(0, loot.Length)]

        let inventory: Inventory =
            inventory
            |> Inventory.update catchId ((+) 1)

        let catches, newCatchSetting =

            if Set.contains catchId player.Catches then
                player.Catches, None
            else
                let catches = player.Catches |> Set.add catchId
                let res = {| TotalCatchesCount = catches.Count |}
                catches, Some res

        let state =
            {
                Players =
                    state.Players
                    |> Players.GuildData.set
                        userId
                        (fun p ->
                            { p with
                                Inventory = inventory
                                Catches = catches
                            }
                        )
            }

        sendCatched newCatchSetting items.[catchId]

        state

let actionReduce (e: EventArgs.MessageCreateEventArgs) (msg: ActionReq) (state: State) =
    match msg with
    | ToFish baitName ->
        awaiti <| e.Channel.TriggerTypingAsync()

        match baitName with
        | Some _ ->
            let send msg =
                let embed = Entities.DiscordEmbedBuilder()
                embed.Color <- Entities.Optional.FromValue(DiscordEmbed.backgroundColorDarkTheme)
                embed.Description <- msg

                let b = Entities.DiscordMessageBuilder()
                b.Embed <- embed.Build()

                awaiti <| e.Channel.SendMessageAsync b

            Actions.toFish baitName send e.Author.Id state

        | None ->
            let player =
                match Players.GuildData.tryFind e.Author.Id state.Players with
                | Some player -> player
                | None -> Players.MainData.Empty

            let baits =
                player.Inventory
                |> Seq.choose (fun (KeyValue(k, v)) ->
                    if v.Count > 0 then
                        let item = items.[v.ItemId]
                        if Array.isEmpty item.Loot then
                            None
                        else
                            Some item
                    else
                        None
                )
                |> List.ofSeq

            BaitChoiceUi.init baits e

            state

    | Inventory ->
        awaiti <| e.Channel.TriggerTypingAsync()

        let b = Entities.DiscordMessageBuilder()

        let userRanks =
            match Players.GuildData.tryFind e.Author.Id state.Players with
            | Some ranking -> ranking.Inventory
            | None -> Map.empty

        InventoryTable.createTable b.AddComponents b.AddEmbed userRanks

        awaiti <| e.Channel.SendMessageAsync b
        state

    | Progress ->
        awaiti <| e.Channel.TriggerTypingAsync()

        let send msg =
            let embed = Entities.DiscordEmbedBuilder()
            embed.Color <- Entities.Optional.FromValue(DiscordEmbed.backgroundColorDarkTheme)
            embed.Description <- msg

            let b = Entities.DiscordMessageBuilder()
            b.Embed <- embed.Build()

            awaiti <| e.Channel.SendMessageAsync b

        let msg =
            match Players.GuildData.tryFind e.Author.Id state.Players with
            | None ->
                createProgressMessage 0
            | Some p ->
                createProgressMessage p.Catches.Count

        send msg

        state

let reduce (msg: Msg) (state: State): State =
    match msg with
    | Request(e, cmd) ->
        match cmd with
        | Request.ActionReq msg ->
            actionReduce e msg state

    | ToFishHandle(e, baitId) ->
        let send msg =
            let embed = Entities.DiscordEmbedBuilder()
            embed.Color <- Entities.Optional.FromValue(DiscordEmbed.backgroundColorDarkTheme)
            embed.Description <- msg
            let b = Entities.DiscordInteractionResponseBuilder()
            b.AddEmbed(embed.Build()) |> ignore
            awaiti <| e.Interaction.CreateResponseAsync(InteractionResponseType.UpdateMessage, b)

        Actions.toFish baitId send e.User.Id state

    | GetState r ->
        r.Reply state

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

let exec e msg =
    m.Post (Request (e, msg))

let componentInteractionCreateHandle client e =
    InventoryTable.componentInteractionCreateHandle
        (fun () -> m.PostAndReply GetState)
        client
        e
    || BaitChoiceUi.handle client e (fun st ->
        m.Post (ToFishHandle(e, st))
    )
