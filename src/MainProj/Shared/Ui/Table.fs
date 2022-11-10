module Shared.Ui.Table
open DSharpPlus
open FsharpMyExtension

open Types
open Extensions

type SortByContainer< ^Case when ^Case: enum<int32> and ^Case: comparison and ^Case: (static member op_Explicit:  ^Case -> int32)> =
    {
        DefaultCase: ^Case
        Cases: (^Case * string) []
        GetDescription: ^Case -> string
        ToString: ^Case -> string
        GetValue: string -> ^Case
    }

    /// `('Case * 'Description) []`
    static member inline Init(caseNames: (^Case * string) []) =
        if Array.isEmpty caseNames then
            failwith "caseNames is empty"

        {
            DefaultCase = fst caseNames.[0]

            Cases = caseNames

            GetDescription =
                let m = Map.ofArray caseNames
                fun case -> m.[case]

            ToString = fun case -> string (int32 case)

            GetValue = fun v -> int32 v |> enum
        }

type Setting<'Item, ^SortBy, 'CustomData, 'State when ^SortBy: enum<int32> and ^SortBy: comparison and ^SortBy: (static member op_Explicit:  ^SortBy -> int32)> =
    {
        Id: string
        GetState: unit -> 'State
        Title: 'CustomData -> 'State -> string
        GetHeaders: ^SortBy -> string []
        GetItems: 'CustomData -> 'State -> 'Item []
        ItemsCountPerPage: int
        SortBy: SortByContainer< ^SortBy>
        SortFunction: ^SortBy -> 'Item [] -> 'Item []
        MapFunction: 'State -> int -> 'Item -> string []
    }

type ComponentId =
    | RefreshButtonId = 0
    | LeftArrowButtonId = 1
    | PaginationButtonId = 2
    | RightArrowButtonId = 3
    | SelectSortId = 4

type InnerSettings<'SortBy, 'CustomData> = {
    [<Newtonsoft.Json.JsonProperty("S")>]
    SortBy: 'SortBy
    [<Newtonsoft.Json.JsonProperty("D")>]
    CustomData: 'CustomData
}

type ComponentState<'SortBy, 'CustomData> =
    Interaction.ComponentState<ComponentId, InnerSettings<'SortBy, 'CustomData>>

let inline createTable
    (addComponents: Entities.DiscordComponent [] -> _)
    addEmbed
    page
    ((sortBy: 'SortBy option), (customData: 'CustomData))
    (setting: Setting<'Item, 'SortBy, 'CustomData, 'State>) =

    let itemsCountPerPage = setting.ItemsCountPerPage
    let state = setting.GetState ()
    let items = setting.GetItems customData state
    let itemsCount = items.Length
    let lastPageItemsCount = itemsCount % itemsCountPerPage
    let pagesCount = itemsCount / itemsCountPerPage + if lastPageItemsCount > 0 then 1 else 0

    let table =
        let sortBy = sortBy |> Option.defaultValue setting.SortBy.DefaultCase
        let headers = setting.GetHeaders sortBy

        let table =
            if 0 < page && page <= pagesCount then
                let lb = (page - 1) * itemsCountPerPage
                let ub =
                    if page = pagesCount then if lastPageItemsCount > 0 then lastPageItemsCount else itemsCountPerPage
                    else itemsCountPerPage

                items
                |> setting.SortFunction sortBy
                |> Seq.skip lb |> Seq.take ub
                |> Seq.mapi ((fun i -> lb + i + 1) >> setting.MapFunction state)
                |> Array.ofSeq
                |> Array.transpose
                |> Array.map (String.concat "\n")

            else
                Array.replicate headers.Length ""

        table
        |> Array.map2 (fun header value -> header, value) headers

    let embed =
        let b =
            Entities.DiscordEmbedBuilder()
                .WithColor(DiscordEmbed.backgroundColorDarkTheme)
                .WithTitle(setting.Title customData state)

        let b =
            table
            |> Array.fold
                (fun (b: Entities.DiscordEmbedBuilder) (header, value) ->
                    // System.ArgumentException: Value cannot be empty or whitespace. (Parameter 'value')
                    let value = if value = "" then "x" else value

                    b.AddField(header, value, true)
                )
                b

        b.Build()

    addEmbed embed |> ignore

    let componentId = setting.Id

    let options =
        setting.SortBy.Cases
        |> Array.map (fun (sortBy, label) ->
            Entities.DiscordSelectComponentOption(label, setting.SortBy.ToString sortBy)
        )

    let createId k =
        let x: ComponentState<_, _> =
            {
                Id = componentId
                ComponentId = k
                Data = {
                    SortBy =
                        sortBy |> Option.defaultValue setting.SortBy.DefaultCase |> setting.SortBy.ToString
                    CustomData = customData
                }
            }
        ComponentState.Serialize x

    addComponents [|
        Entities.DiscordSelectComponent(
            createId ComponentId.SelectSortId,
            "Отсортировать по",
            options,
            false
        )
    |]
    |> ignore

    addComponents [|
        Entities.DiscordButtonComponent(
            ButtonStyle.Secondary,
            createId ComponentId.RefreshButtonId,
            "",
            emoji = Entities.DiscordComponentEmoji(Name = "🔄") // :arrows_counterclockwise:
        )

        Entities.DiscordButtonComponent(
            ButtonStyle.Secondary,
            createId ComponentId.LeftArrowButtonId,
            "",
            disabled = (page <= 1),
            emoji = Entities.DiscordComponentEmoji(Name = "⬅️")
        )
        Entities.DiscordButtonComponent(
            ButtonStyle.Secondary,
            createId ComponentId.PaginationButtonId,
            sprintf "%d/%d" page pagesCount,
            disabled = true
        )
        Entities.DiscordButtonComponent(
            ButtonStyle.Secondary,
            createId ComponentId.RightArrowButtonId,
            "",
            disabled = (page >= pagesCount),
            emoji = Entities.DiscordComponentEmoji(Name = "➡️")
        )
    |]
    |> ignore

let inline componentInteractionCreateHandle
    (client: DiscordClient)
    (e: EventArgs.ComponentInteractionCreateEventArgs)
    (setting: Setting<'Item, 'SortBy, 'CustomData, 'State>) =

    let getCurrentPage () =
        e.Message.Components
        |> Seq.tryPick (fun row ->
            row.Components
            |> Seq.tryPick (fun x ->
                let id = x.CustomId

                match Interaction.ComponentState.TryDeserialize setting.Id id with
                | Some res ->
                    match res with
                    | Ok (state: ComponentState<'SortBy, 'CustomData>) ->
                        match state.ComponentId with
                        | ComponentId.PaginationButtonId ->
                            let paginationButton = x :?> Entities.DiscordButtonComponent
                            let page =
                                let label = paginationButton.Label
                                let slashIndex = label.IndexOf "/"
                                int label.[..slashIndex - 1]
                            Some page
                        | x ->
                            None
                    | _ ->
                        None
                | _ -> None
            )
        )

    let update mapPage (innerData: InnerSettings<'SortBy, 'CustomData>) = // (sortBy: 'SortBy option) customData =
        let currentPage =
            match getCurrentPage () with
            | Some currentPage -> currentPage

            | None -> 1

        let b = Entities.DiscordInteractionResponseBuilder()

        createTable b.AddComponents b.AddEmbed (mapPage currentPage) (Some innerData.SortBy, innerData.CustomData) setting

        awaiti <| e.Interaction.CreateResponseAsync(InteractionResponseType.UpdateMessage, b)

    let restartComponent errMsg =
        DiscordMessage.Ext.clearComponents e.Message

        let b = Entities.DiscordInteractionResponseBuilder()
        b.Content <-
            [
                "Вызовите комманду еще раз, потому что-то пошло не так:"
                "```"
                sprintf "%s" errMsg
                "```"
            ] |> String.concat "\n"

        b.IsEphemeral <- true

        awaiti <| e.Interaction.CreateResponseAsync(InteractionResponseType.ChannelMessageWithSource, b)

    if e.Message.Author.Id = client.CurrentUser.Id then
        match Interaction.ComponentState.TryDeserialize setting.Id e.Id with
        | Some res ->
            match res with
            | Ok (data: ComponentState<'SortBy, 'CustomData>) ->
                match data.ComponentId with
                | ComponentId.RefreshButtonId ->
                    update id data.Data
                | ComponentId.LeftArrowButtonId ->
                    update (fun currentPage -> currentPage - 1) data.Data
                | ComponentId.RightArrowButtonId ->
                    update (fun currentPage -> currentPage + 1) data.Data
                | ComponentId.SelectSortId ->
                    match e.Values with
                    | [| sortByRaw |] ->
                        { data.Data with
                            SortBy = setting.SortBy.GetValue sortByRaw
                        }
                        |> update id
                    | xs ->
                        sprintf "expected [| sortByRaw |] but %A" xs
                        |> restartComponent
                | x ->
                    sprintf "expected data.ComponentId but %A" x
                    |> restartComponent
            | Error errMsg ->
                restartComponent errMsg

            true
        | _ -> false
    else
        false
