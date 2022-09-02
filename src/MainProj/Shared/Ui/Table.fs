module Shared.Ui.Table
open DSharpPlus

open Types

type Setting<'Item, 'SortBy> =
    {
        Id: string
        Title: string
        GetHeaders: 'SortBy -> string []
        GetItems: unit -> 'Item []
        ItemsCountPerPage: int
        SortBy: 'SortBy
        SortFunction: 'SortBy -> 'Item [] -> 'Item []
        MapFunction: int -> 'Item -> string []
    }

[<Literal>]
let RefreshButtonId = "RefreshButtonId"
[<Literal>]
let LeftArrowButtonId = "LeftArrowButtonId"
[<Literal>]
let PaginationButtonId = "PaginationButtonId"
[<Literal>]
let RightArrowButtonId = "RightArrowButtonId"

let createTable
    (addComponents: Entities.DiscordComponent [] -> _)
    addEmbed
    page
    (setting: Setting<'Item, 'SortBy>) =

    let itemsCountPerPage = setting.ItemsCountPerPage
    let items = setting.GetItems ()
    let itemsCount = items.Length
    let lastPageItemsCount = itemsCount % itemsCountPerPage
    let pagesCount = itemsCount / itemsCountPerPage + if lastPageItemsCount > 0 then 1 else 0

    let table =
        let headers = setting.GetHeaders setting.SortBy

        let table =
            if 0 < page && page <= pagesCount then
                let lb = (page - 1) * itemsCountPerPage
                let ub =
                    if page = pagesCount then if lastPageItemsCount > 0 then lastPageItemsCount else itemsCountPerPage
                    else itemsCountPerPage

                items
                |> setting.SortFunction setting.SortBy
                |> Seq.skip lb |> Seq.take ub
                |> Seq.mapi (fun i -> setting.MapFunction (lb + i + 1))
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
                .WithColor(Entities.DiscordColor "#2f3136")
                .WithTitle(setting.Title)

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

    let componentId = setting.Id

    addComponents [|
        Entities.DiscordButtonComponent(
            ButtonStyle.Secondary,
            componentId + RefreshButtonId,
            "",
            emoji = Entities.DiscordComponentEmoji(Name = "🔄") // :arrows_counterclockwise:
        )

        Entities.DiscordButtonComponent(
            ButtonStyle.Secondary,
            componentId + LeftArrowButtonId,
            "",
            disabled = (page <= 1),
            emoji = Entities.DiscordComponentEmoji(Name = "⬅️")
        )
        Entities.DiscordButtonComponent(
            ButtonStyle.Secondary,
            componentId + PaginationButtonId,
            sprintf "%d/%d" page pagesCount,
            disabled = true
        )
        Entities.DiscordButtonComponent(
            ButtonStyle.Secondary,
            componentId + RightArrowButtonId,
            "",
            disabled = (page >= pagesCount),
            emoji = Entities.DiscordComponentEmoji(Name = "➡️")
        )
    |]
    |> ignore

    addEmbed embed |> ignore

let componentInteractionCreateHandle
    (client: DiscordClient)
    (e: EventArgs.ComponentInteractionCreateEventArgs)
    (setting: Setting<'Item, 'SortBy>) =

    let getCurrentPage () =
        let PaginationButtonId = setting.Id + PaginationButtonId
        e.Message.Components
        |> Seq.tryPick (fun row ->
            row.Components
            |> Seq.tryPick (fun row ->
                if row.CustomId = PaginationButtonId then
                    let paginationButton = row :?> Entities.DiscordButtonComponent
                    let page =
                        let label = paginationButton.Label
                        let slashIndex = label.IndexOf "/"
                        int label.[..slashIndex - 1]
                    Some page
                else
                    None
            )
        )

    let update getPage =
        let currentPage =
            match getCurrentPage () with
            | Some currentPage -> currentPage

            | None -> 1

        let b = Entities.DiscordInteractionResponseBuilder()

        createTable b.AddComponents b.AddEmbed (getPage currentPage) setting

        awaiti <| e.Interaction.CreateResponseAsync(InteractionResponseType.UpdateMessage, b)

    if e.Message.Author.Id = client.CurrentUser.Id then
        if e.Id.StartsWith setting.Id then
            match e.Id.[setting.Id.Length..] with
            | RefreshButtonId ->
                update id
                true
            | LeftArrowButtonId ->
                update (fun currentPage -> currentPage - 1)
                true
            | RightArrowButtonId ->
                update (fun currentPage -> currentPage + 1)
                true
            | _ -> false
        else
            false
    else
        false
