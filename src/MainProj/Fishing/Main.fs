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

type DataOrUrl =
    | Data of string
    | Url of string

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
        let setSettings = "fishingSetSettings"
        let setItems = "fishingSetItems"

    let psetting: _ Parser =
        let psetSetting =
            skipStringCI CommandNames.setSettings .>> spaces
            >>. (pcodeBlock <|> manySatisfy (fun _ -> true))

        psetSetting |>> SetSettings

    let pitems: _ Parser =
        let pdataOrUrl =
            let purl =
                skipString "http"
                >>. many1Satisfy ((<>) ' ')
                |>> sprintf "http%s"

            (purl |>> Url)
            <|> (pcodeBlock <|> many1Satisfy (fun _ -> true) |>> Data)

        let psetItems =
            skipStringCI CommandNames.setItems .>> spaces
            >>. opt pdataOrUrl

        psetItems |>> SetItems

    let paction: _ Parser =
        let toFish =
            skipStringCI CommandNames.toFish .>> spaces
            >>. opt (many1Satisfy (fun _ -> true))

        choice [
            toFish |>> ToFish
            skipStringCI CommandNames.inventory >>% Inventory
            skipStringCI CommandNames.progress >>% Progress
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
                    match Players.GuildData.tryFind userId state.Players with
                    | Some ranking -> ranking.Inventory
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
            let image = catch.Data.ImageUrl
            if not (System.String.IsNullOrEmpty image) then
                setImage image

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
            let settings = state.Settings.Cache

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

        let useBaseBaitOrContinue (baseCatch: ItemsDb.ItemT) next =
            match baitId with
            | Some baitName ->
                next baitName
            | None ->
                let player =
                    match Players.GuildData.tryFind userId state.Players with
                    | Some player -> player
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

        let itemIsBait (item: ItemsDb.ItemT) next =
            if ItemsDb.Item.isBait item then
                next ()
            else
                let msg = sprintf "На \"%A\" больше ничего словить нельзя." item.Data.Name
                send msg

                state

        let sendThereIsNoMoreBait (bait: ItemsDb.ItemT) =
            let msg = sprintf "Наживок \"%s\" в инвентаре больше нет." bait.Data.Name
            send msg

            state

        let getCurrentPlayer bait next =
            match Players.GuildData.tryFind userId state.Players with
            | Some player ->
                next player
            | None ->
                sendThereIsNoMoreBait bait

        let getBaitFromInventory (bait: ItemsDb.ItemT) (player: Players.MainData) next =
            match Map.tryFind bait.Id player.Inventory with
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
        itemIsBait bait <| fun () ->
        getCurrentPlayer bait <| fun player ->
        getBaitFromInventory bait player <| fun baitInInventory ->

        let loot = bait.Data.Loot
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

            match ItemsDb.Items.tryFindByName baitName state.Items with
            | Some baitName ->
                Actions.toFish (Some baitName.Id) send setImage e.Author.Id state
            | None ->
                send (sprintf "Предмет \"%s\" в игре не найден." baitName)

                state

        | None ->
            let player =
                match Players.GuildData.tryFind e.Author.Id state.Players with
                | Some player -> player
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
            match Players.GuildData.tryFind e.Author.Id state.Players with
            | None ->
                createProgressMessage 0 state.Items.Cache.Count
            | Some p ->
                createProgressMessage p.Catches.Count state.Items.Cache.Count

        send msg

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

                let getJson (json: DataOrUrl option) next =
                    let download url =
                        let res =
                            WebDownloader.tryGet id url
                            |> Async.RunSynchronously
                        match res with
                        | Right x ->
                            match x.Content with
                            | WebDownloader.Text json ->
                                next json
                            | _ ->
                                send (sprintf "Expected WebDownloader.Text but binary.")
                                state
                        | Left x ->
                            send (sprintf "%A" x)
                            state

                    match json with
                    | Some json ->
                        match json with
                        | Url url ->
                            download url
                        | Data json ->
                            next json

                    | None ->
                        match Seq.tryHead e.Message.Attachments with
                        | Some jsonFile ->
                            // if jsonFile.MediaType = "application/json; charset=utf-8" then
                            download jsonFile.Url
                        | None ->
                            send "Нужно указать настройки либо явно, либо прикрепить файл к сообщению."
                            state

                let deserializeItemsArray json next =
                    // match ItemsDb.ItemsArray.tryDeserialize json with
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
        Players = Players.GuildData.init Db.database
        Items = ItemsDb.Items.init Db.database
        Settings = Settings.GuildData.init Db.database
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
