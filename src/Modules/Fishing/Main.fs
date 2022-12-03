module Fishing.Main
open DSharpPlus
open FsharpMyExtension
open FsharpMyExtension.Either

open Types
open Extensions
open Model

type State =
    {
        Items: ItemsDb.Items
        Settings: Settings.GuildData
        Players: Players.GuildData
    }

type ActionReq =
    | ToFish of baitName: string option
    | Inventory
    | Progress
    | Inspect of itemName: string
    | OpenUp of itemName: string

type ItemsReq =
    | SetItems of json: DataOrUrl option

type SettingsReq =
    | SetSettings of json: string

type Request =
    | ActionReq of ActionReq
    | ItemsReq of ItemsReq
    | SettingsReq of SettingsReq

module Parser =
    open FParsec

    open DiscordMessage.Parser

    type 'Result Parser = Primitives.Parser<'Result, unit>

    module CommandNames =
        let toFish = "рыбачить"
        let inventory = "инвентарь"
        let progress = "прогресс"
        let inspect = "осм"
        let openUp = "открыть"
        let setSettings = "fishingSetSettings"
        let setItems = "fishingSetItems"

    let psetting: _ Parser =
        let psetSetting =
            skipStringCI CommandNames.setSettings .>> spaces
            >>. (pcodeBlock <|> manySatisfy (fun _ -> true))

        psetSetting |>> SetSettings

    let pitems: _ Parser =
        let psetItems =
            skipStringCI CommandNames.setItems .>> spaces
            >>. opt DataOrUrl.Parser.parser

        psetItems |>> SetItems

    let paction: _ Parser =
        let pitemName =
            many1Satisfy (fun _ -> true)

        let toFish =
            skipStringCI CommandNames.toFish .>> spaces
            >>. opt pitemName

        let pinspect =
            skipStringCI CommandNames.inspect .>> spaces
            >>. pitemName

        let popen: _ Parser =
            skipStringCI CommandNames.openUp .>> spaces
            >>. pitemName

        choice [
            toFish |>> ToFish
            skipStringCI CommandNames.inventory >>% Inventory
            skipStringCI CommandNames.progress >>% Progress
            pinspect |>> Inspect
            popen |>> OpenUp
        ]

    let start f: _ Parser =
        choice [
            paction |>> Request.ActionReq
            psetting |>> Request.SettingsReq
            pitems |>>  Request.ItemsReq
        ]
        >>= fun msg ->
            preturn (fun x -> f x msg)

type SelectionBaitKey = Option<ItemId>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module SelectionBaitKey =
    let serialize (data: SelectionBaitKey) =
        data |> Json.ser

    let tryDeserialize json =
        try
            let res: SelectionBaitKey = Json.des json
            Ok res
        with e ->
            Error e.Message

type Msg =
    | Request of EventArgs.MessageCreateEventArgs * Request
    | ToFishHandle of EventArgs.ComponentInteractionCreateEventArgs * baitId: SelectionBaitKey
    | GetState of AsyncReplyChannel<State>

module InventoryTable =
    open Shared.Ui.Table

    type SortBy =
        | SortByName = 0
        | SortByCount = 1

    let initSetting getInventory: Setting<_, SortBy, _, (UserId * State)> =
        {
            Id = "InventoryTable"

            GetState = getInventory

            Title = fun _ _ -> "Инвентарь"

            GetHeaders = fun sortBy ->
                match sortBy with
                | SortBy.SortByName ->
                    [| "Название▼"; "Кол-во" |]
                | SortBy.SortByCount ->
                    [| "Учасники"; "Кол-во▼" |]
                | x ->
                    failwithf "InventoryTable.SortBy %A" x

            GetItems = fun () (userId, state) ->
                let inventory =
                    match Players.GuildData.tryFindById userId state.Players with
                    | Some ranking -> ranking.Data.Inventory
                    | None -> Map.empty

                inventory
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
                fun (_, state) i (itemId, user) ->
                    let getItemById itemId = ItemsDb.Items.tryFindById itemId state.Items

                    let itemName =
                        match getItemById itemId with
                        | Some item -> item.Data.Name
                        | None ->
                            sprintf "Неизвестный предмет %A" itemId

                    [|
                        itemName
                        string user.Count
                    |]
        }

    let createTable
        (addComponents: Entities.DiscordComponent [] -> _)
        addEmbed
        getState =

        createTable addComponents addEmbed 1 (None, ()) (initSetting getState)

    let componentInteractionCreateHandle getState (client: DiscordClient) (e: EventArgs.ComponentInteractionCreateEventArgs) =
        let getState () =
            let state: State = getState ()
            e.User.Id, state

        componentInteractionCreateHandle client e (initSetting getState)

let createProgressMessage totalCatchesCount itemsCount =
    sprintf "Найдено %d обитателей морских глубин из %d."
        totalCatchesCount
        itemsCount

module BaitChoiceUi =
    type ComponentId =
        | BaitChoiceId = 0

    type Data =
        {
            OwnerId: UserId
        }

    type BaitComponentState = Interaction.ComponentState<ComponentId, Data>

    let messageTypeId = "BaitChoiceComponentId"

    let init (baits: ItemsDb.ItemT list) (e: EventArgs.MessageCreateEventArgs) =
        let userId = e.Author.Id

        let embed = Entities.DiscordEmbedBuilder()
        embed.Color <- Entities.Optional.FromValue(DiscordEmbed.backgroundColorDarkTheme)
        embed.Description <- sprintf "<@%d>, выбери наживку:" userId

        let b = DSharpPlus.Entities.DiscordMessageBuilder()
        b.Embed <- embed.Build()

        let options =
            let withoutBaitOption =
                let name = "Рыбачить без наживки"
                let value: SelectionBaitKey = None
                Entities.DiscordSelectComponentOption(name, SelectionBaitKey.serialize value)

            let baitOptions =
                baits
                |> List.mapi (fun i item ->
                    let selectionBaitKey = Some item.Id
                    Entities.DiscordSelectComponentOption(item.Data.Name, SelectionBaitKey.serialize selectionBaitKey)
                )

            withoutBaitOption::baitOptions

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
                            match e.Values with
                            | [|rawSelectedBaitKey|] ->
                                match SelectionBaitKey.tryDeserialize rawSelectedBaitKey with
                                | Ok selectedBait ->
                                    next selectedBait
                                | Error errMsg ->
                                    sprintf "SelectionBaitKey.tryDeserialize return error:\n%A" errMsg
                                    |> restartComponent
                            | xs ->
                                sprintf "expected e.Values = [|rawSelectedBaitKey|] but %A" xs
                                |> restartComponent
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
    let toFish baitId send setImage (userId: UserId) (state: State) =
        let items = state.Items
        let itemsCount = items.Cache.Count

        let sendCatched (newCatchSetting: option<{| TotalCatchesCount: int |}>) (catch: ItemsDb.ItemT) =
            catch.Data.ImageUrl
            |> Option.iter (fun image ->
                setImage image
            )

            [
                sprintf "Поймано \"%s\"!" catch.Data.Name

                let description = catch.Data.Description
                if not (System.String.IsNullOrEmpty description) then
                    ""
                    description

                match newCatchSetting with
                | Some newCatchSetting ->
                    ""
                    createProgressMessage newCatchSetting.TotalCatchesCount itemsCount
                | None -> ()
            ]
            |> String.concat "\n"
            |> send

        let getItem itemId next =
            match ItemsDb.Items.tryFindById itemId items with
            | Some bait ->
                next bait
            | None ->
                let msg = sprintf "Предмет %A в игре не найден. Пожалуйста, свяжитесь с администратором." itemId
                send msg

                state

        let getBaseСatch next =
            match Settings.GuildData.tryFindById () state.Settings with
            | Some settings ->
                match settings.Data.BaseCatchId with
                | Some baseCatchId ->
                    match ItemsDb.Items.tryFindById baseCatchId items with
                    | Some baseCatch ->
                        next baseCatch
                    | None ->
                        send "Начальный улов определен, но сам предмет в базе данных отсутствует. Пожалуйста, свяжитесь с администратором."
                        state
                | None ->
                    send "Начальный улов не определен! Пожалуйста, свяжитесь с администратором."
                    state
            | None ->
                send "Начальный улов не определен! Пожалуйста, свяжитесь с администратором."
                state

        let useBaseBaitOrContinue (baseCatch: ItemsDb.ItemT) next =
            match baitId with
            | Some baitName ->
                next baitName
            | None ->
                let player =
                    match Players.GuildData.tryFindById userId state.Players with
                    | Some player -> player.Data
                    | None -> Players.MainData.Empty

                let inventory: Inventory =
                    player.Inventory
                    |> Inventory.update baseCatch.Id ((+) 1)

                let state =
                    { state with
                        Players =
                            state.Players
                            |> Players.GuildData.set
                                userId
                                (fun p -> { p with Inventory = inventory })
                    }

                sendCatched None baseCatch

                state

        let baitGetLoot (item: ItemsDb.ItemT) next =
            match item.Data.AsBait with
            | Some loot ->
                next loot
            | None ->
                let msg = sprintf "На \"%A\" больше ничего словить нельзя." item.Data.Name
                send msg

                state

        let sendThereIsNoMoreBait (bait: ItemsDb.ItemT) =
            let msg = sprintf "Наживок \"%s\" в инвентаре больше нет." bait.Data.Name
            send msg

            state

        let getCurrentPlayer bait next =
            match Players.GuildData.tryFindById userId state.Players with
            | Some player ->
                next player
            | None ->
                sendThereIsNoMoreBait bait

        let getBaitFromInventory (bait: ItemsDb.ItemT) (player: Players.Player) next =
            match Map.tryFind bait.Id player.Data.Inventory with
            | Some baitInInventory ->
                if baitInInventory.Count > 0 then
                    next baitInInventory
                else
                    sendThereIsNoMoreBait bait
            | None ->
                sendThereIsNoMoreBait bait

        getBaseСatch <| fun baseCatch ->
        useBaseBaitOrContinue baseCatch <| fun baitId ->
        getItem baitId <| fun bait ->
        baitGetLoot bait <| fun loot ->
        getCurrentPlayer bait <| fun player ->
        getBaitFromInventory bait player <| fun baitInInventory ->
        let player = player.Data

        let catchId = loot.[r.Next(0, loot.Length)]

        getItem catchId <| fun catch ->
        // remove bait from inventory
        let inventory: Inventory =
            player.Inventory
            |> Inventory.update bait.Id (fun count -> count - 1)

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
            { state with
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

        sendCatched newCatchSetting catch

        state

let actionReduce (e: EventArgs.MessageCreateEventArgs) (msg: ActionReq) (state: State) =
    let send msg =
        let embed = Entities.DiscordEmbedBuilder()
        embed.Color <- Entities.Optional.FromValue(DiscordEmbed.backgroundColorDarkTheme)
        embed.Description <- msg

        let b = Entities.DiscordMessageBuilder()
        b.Embed <- embed.Build()

        awaiti <| e.Channel.SendMessageAsync b

    let getItemByName itemName next =
        match ItemsDb.Items.tryFindByName itemName state.Items with
        | Some item ->
            next item
        | None ->
            send (sprintf "Предмет \"%s\" в игре не найден." itemName)
            state

    match msg with
    | ToFish baitName ->
        awaiti <| e.Channel.TriggerTypingAsync()

        match baitName with
        | Some baitName ->
            let embed = Entities.DiscordEmbedBuilder()
            let setImage imgUrl = embed.ImageUrl <- imgUrl
            let send msg =
                embed.Color <- Entities.Optional.FromValue(DiscordEmbed.backgroundColorDarkTheme)
                embed.Description <- msg

                let b = Entities.DiscordMessageBuilder()
                b.Embed <- embed.Build()

                awaiti <| e.Channel.SendMessageAsync b

            getItemByName baitName <| fun bait ->
                Actions.toFish (Some bait.Id) send setImage e.Author.Id state

        | None ->
            let player =
                match Players.GuildData.tryFindById e.Author.Id state.Players with
                | Some player -> player.Data
                | None -> Players.MainData.Empty

            let baits =
                player.Inventory
                |> Seq.choose (fun (KeyValue(itemId, invItem)) ->
                    if invItem.Count > 0 then
                        ItemsDb.Items.tryFindById itemId state.Items
                        |> Option.bind (fun item ->
                            if ItemsDb.Item.isBait item then
                                Some item
                            else
                                None
                        )
                    else
                        None
                )
                |> List.ofSeq

            BaitChoiceUi.init baits e

            state

    | Inventory ->
        awaiti <| e.Channel.TriggerTypingAsync()

        let b = Entities.DiscordMessageBuilder()

        let getState () =
            e.Author.Id, state

        InventoryTable.createTable b.AddComponents b.AddEmbed getState

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
            match Players.GuildData.tryFindById e.Author.Id state.Players with
            | None ->
                createProgressMessage 0 state.Items.Cache.Count
            | Some p ->
                createProgressMessage p.Data.Catches.Count state.Items.Cache.Count

        send msg

        state

    | Inspect itemName ->
        awaiti <| e.Channel.TriggerTypingAsync()

        getItemByName itemName <| fun item ->

        let embed = Entities.DiscordEmbedBuilder()
        embed.Color <- Entities.Optional.FromValue(DiscordEmbed.backgroundColorDarkTheme)

        let description = item.Data.Description
        if not (System.String.IsNullOrEmpty description) then
            embed.Description <- description

        embed.Title <- item.Data.Name

        item.Data.ImageUrl
        |> Option.iter (fun imageUrl ->
            embed.ImageUrl <- imageUrl
        )

        let b = Entities.DiscordMessageBuilder()
        b.Embed <- embed.Build()

        awaiti <| e.Channel.SendMessageAsync b

        state

    | OpenUp itemName ->
        awaiti <| e.Channel.TriggerTypingAsync()

        let embed = Entities.DiscordEmbedBuilder()
        let setImage imgUrl = embed.ImageUrl <- imgUrl
        let send msg =
            embed.Color <- Entities.Optional.FromValue(DiscordEmbed.backgroundColorDarkTheme)
            embed.Description <- msg

            let b = Entities.DiscordMessageBuilder()
            b.Embed <- embed.Build()

            awaiti <| e.Channel.SendMessageAsync b

        let userId = e.Author.Id

        let items = state.Items
        let itemsCount = items.Cache.Count

        let getItem itemId next =
            match ItemsDb.Items.tryFindById itemId items with
            | Some bait ->
                next bait
            | None ->
                let msg = sprintf "Предмет %A в игре не найден. Пожалуйста, свяжитесь с администратором." itemId
                send msg

                state

        let getChestLoot (item: ItemsDb.ItemT) next =
            match item.Data.AsChest with
            | Some loot ->
                next loot
            | None ->
                sprintf "<@%d>, этот предмет нельзя открыть." userId
                |> send

                state

        let sendDontHaveItem (item: ItemsDb.ItemT) =
            sprintf "<@%d>, в инвентаре нет \"%s\"" userId item.Data.Name
            |> send

        let getPlayer (item: ItemsDb.ItemT) next =
            match Players.GuildData.tryFindById userId state.Players with
            | Some player -> next player
            | None ->
                sendDontHaveItem item
                state

        let getInventoryItem (item: ItemsDb.ItemT) (player: Players.Player) next =
            match Inventory.tryFindById item.Id player.Data.Inventory with
            | Some inv -> next inv
            | None ->
                sendDontHaveItem item
                state

        let isEnoughItems itemName (invItem: InventoryItem) next =
            if invItem.Count > 0 then
                next ()
            else
                sprintf "<@%d>, в инвентаре не хватает \"%s\"." userId itemName
                |> send
                state

        let sendCatched (newCatchSetting: option<{| TotalCatchesCount: int |}>) (catch: ItemsDb.ItemT) =
            catch.Data.ImageUrl
            |> Option.iter (fun image ->
                setImage image
            )

            [
                sprintf "В сундуке оказалось \"%s\"!" catch.Data.Name

                let description = catch.Data.Description
                if not (System.String.IsNullOrEmpty description) then
                    ""
                    description

                match newCatchSetting with
                | Some newCatchSetting ->
                    ""
                    createProgressMessage newCatchSetting.TotalCatchesCount itemsCount
                | None -> ()
            ]
            |> String.concat "\n"
            |> send

        getItemByName itemName <| fun chest ->
        getChestLoot chest <| fun loot ->
        getPlayer chest <| fun player ->
        getInventoryItem chest player <| fun invItem ->
        isEnoughItems chest.Data.Name invItem <| fun () ->

        // remove the chest from the inventory
        let inventory =
            player.Data.Inventory
            |> Inventory.update chest.Id (fun count -> count - 1)

        let catchId = loot.[r.Next(0, loot.Length)]

        let inventory: Inventory =
            inventory
            |> Inventory.update catchId ((+) 1)

        let catches, newCatchSetting =
            if Set.contains catchId player.Data.Catches then
                player.Data.Catches, None
            else
                let catches = player.Data.Catches |> Set.add catchId
                let res = {| TotalCatchesCount = catches.Count |}
                catches, Some res

        let state =
            { state with
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

        getItem catchId <| fun catch ->
        sendCatched newCatchSetting catch

        state

let reduce (msg: Msg) (state: State): State =
    match msg with
    | Request(e, cmd) ->
        let send msg =
            let embed = Entities.DiscordEmbedBuilder()
            embed.Color <- Entities.Optional.FromValue(DiscordEmbed.backgroundColorDarkTheme)
            embed.Description <- msg

            let b = Entities.DiscordMessageBuilder()
            b.Embed <- embed.Build()

            awaiti <| e.Channel.SendMessageAsync b

        let isBotOwner (user: Entities.DiscordUser) next =
            if user.Id = Db.superUserId then
                next ()
            else
                send (sprintf "Только <@%d> может пользоваться этой командой." Db.superUserId)
                state

        match cmd with
        | Request.ActionReq msg ->
            actionReduce e msg state

        | Request.ItemsReq msg ->
            match msg with
            | SetItems json ->
                awaiti <| e.Channel.TriggerTypingAsync()

                let getJson json next =
                    match DataOrUrl.getOrAttachment e.Message json with
                    | Ok x -> next x
                    | Error errMsg ->
                        send errMsg
                        state

                let deserializeItemsArray json next =
                    match Editor.Types.Items.tryDeserialize json with
                    | Ok itemsArray ->
                        itemsArray
                        |> Array.map ItemsDb.Item.ofEditorItem
                        |> next

                    | Error errMsg ->
                        send (sprintf "tryDeserialize: %A" errMsg)
                        state

                isBotOwner e.Author <| fun () ->
                getJson json <| fun json ->
                deserializeItemsArray json <| fun itemsArray ->

                let state =
                    { state with
                        Items =
                            ItemsDb.Items.setItems itemsArray state.Items
                    }

                send "Done!"

                state

        | Request.SettingsReq msg ->
            match msg with
            | SetSettings json ->
                let deserializeSettings json next =
                    match Settings.MainData.Deserialize json with
                    | Ok itemsArray ->
                        next itemsArray
                    | Error errMsg ->
                        send (sprintf "%A" errMsg)
                        state

                isBotOwner e.Author <| fun () ->
                deserializeSettings json <| fun settings ->

                let state =
                    { state with
                        Settings =
                            Settings.GuildData.set (fun _ -> settings) state.Settings
                    }

                send "Done!"

                state

    | ToFishHandle(e, baitId) ->
        let embed = Entities.DiscordEmbedBuilder()
        let setImage imgUrl = embed.ImageUrl <- imgUrl
        let send msg =
            embed.Color <- Entities.Optional.FromValue(DiscordEmbed.backgroundColorDarkTheme)
            embed.Description <- msg
            let b = Entities.DiscordInteractionResponseBuilder()
            b.AddEmbed(embed.Build()) |> ignore
            awaiti <| e.Interaction.CreateResponseAsync(InteractionResponseType.UpdateMessage, b)

        Actions.toFish baitId send setImage e.User.Id state

    | GetState r ->
        r.Reply state

        state

let m =
    let init: State = {
        Players = Players.GuildData.init "fishingPlayers" Db.database
        Items = ItemsDb.Items.init "fishingItems" Db.database
        Settings = Settings.GuildData.init "fishingSettings" Db.database
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
    InventoryTable.componentInteractionCreateHandle
        (fun () -> m.PostAndReply GetState)
        client
        e
    || BaitChoiceUi.handle client e (fun st ->
        m.Post (ToFishHandle(e, st))
    )
