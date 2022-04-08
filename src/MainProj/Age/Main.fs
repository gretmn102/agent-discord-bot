module Age.Main
open FsharpMyExtension
open FsharpMyExtension.Either
open DSharpPlus

open Types

type Request =
    | CreateForm

module Parser =
    open FParsec

    open DiscordMessage.Parser

    type 'Result Parser = Primitives.Parser<'Result, unit>

    let start: _ Parser =
        pstring "ageCreateForm" >>% CreateForm

[<Literal>]
let AgeCreateModalButtonId = "ageCreateModalButtonId"

[<Literal>]
let AgeInputComponentId = "ageInputComponentId"

[<Literal>]
let AgeModalId = "ageModalId"

[<Literal>]
let AgeGetAgeStatisticsButtonId = "ageGetAgeStatisticsButtonId"

type State =
    {
        Users: Model.Age.Users
    }

type Req =
    | Request of EventArgs.MessageCreateEventArgs * Request
    | InputAge of UserId * GuildId * int
    /// `age * count`
    | GetAgeStatistics of AsyncReplyChannel<Map<int, int>> * GuildId
    | IsUserEnteredAge of AsyncReplyChannel<bool> * UserId
    | RemoveGuildId of UserId * GuildId

let reduce (msg: Req) (state: State) =
    match msg with
    | InputAge (userId, guildId, age) ->
        let userData =
            match Map.tryFind userId state.Users with
            | Some userData ->
                let userData =
                    { userData with
                        Age = age
                        GuildIds = Set.add guildId userData.GuildIds
                    }
                Model.Age.replace userData

                userData
            | None ->
                Model.Age.insert(userId, age, Set.singleton guildId)

        { state with
            Users =
                Map.add userId userData state.Users
        }

    | GetAgeStatistics (r, guildId) ->
        state.Users
        |> Map.fold
            (fun st userId user ->
                if Set.contains guildId user.GuildIds then
                    st
                    |> Map.addOrModWith
                        user.Age
                        (fun () -> 1)
                        (fun acc -> 1 + acc)
                else
                    st
            )
            Map.empty
        |> r.Reply

        state

    | IsUserEnteredAge(r, userId) ->
        Map.containsKey userId state.Users
        |> r.Reply

        state

    | RemoveGuildId(userId, guildId) ->
        match Map.tryFind userId state.Users with
        | None -> state
        | Some userData ->
            let userData =
                { userData with
                    GuildIds =
                        Set.remove guildId userData.GuildIds
                }

            Model.Age.replace userData

            { state with
                Users =
                    Map.add userId userData state.Users
            }

    | Request(e, msg) ->
        match msg with
        | CreateForm ->
            e.Channel.TriggerTypingAsync().GetAwaiter().GetResult()

            let b = Entities.DiscordMessageBuilder()

            b.Embed <-
                Entities.DiscordEmbedBuilder(
                    Color = Entities.Optional.FromValue(Entities.DiscordColor("#2f3136")),
                    Title = "Анонимный опросник возраста"
                ).Build()

            let buttons: Entities.DiscordComponent [] =
                [|
                    Entities.DiscordButtonComponent(
                        ButtonStyle.Secondary,
                        AgeCreateModalButtonId,
                        "Ввести возраст",
                        emoji = Entities.DiscordComponentEmoji(Name = "✍️")
                    )
                    Entities.DiscordButtonComponent(
                        ButtonStyle.Secondary,
                        AgeGetAgeStatisticsButtonId,
                        "Посмотреть статистику",
                        emoji = Entities.DiscordComponentEmoji(Name = "📈")
                    )
                |]

            buttons
            |> b.AddComponents
            |> ignore

            awaiti (e.Channel.SendMessageAsync b)

            state

let m: MailboxProcessor<Req> =
    let init = {
        Users = Model.Age.getAll ()
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

let createAgeStatisticEmbed guildId =
    let ages, counts =
        let ageStatistic = m.PostAndReply(fun r -> GetAgeStatistics(r, guildId))

        let f = List.rev >> String.concat "\n"
        ageStatistic
        |> Seq.fold
            (fun (ages, counts) (KeyValue(age, count)) -> string age::ages, string count::counts)
            ([], [])
        |> fun (ages, counts) -> f ages, f counts


    Entities.DiscordEmbedBuilder()
        .WithDescription("Статистика сервера такова:")
        .AddField("Возраст", ages, true)
        .AddField("Кол-во", counts, true)
        .Build()

let modalHandle (e: EventArgs.ModalSubmitEventArgs) =
    let interaction = e.Interaction
    if interaction.Data.CustomId = AgeModalId then
        match e.Values.TryGetValue AgeInputComponentId with
        | true, age ->
            let b = Entities.DiscordInteractionResponseBuilder()
            b.IsEphemeral <- true

            match Int32.tryParse age with
            | None ->
                let content = sprintf "Пожалуйста, введите число в поле \"Возраст\", а не `%s`." age
                b.Content <- content
            | Some age ->
                m.Post (InputAge (interaction.User.Id, interaction.Guild.Id, age))

                b.Content <- "Спасибо, что указали возраст."

                b.AddEmbed (createAgeStatisticEmbed interaction.Guild.Id) |> ignore

            interaction.CreateResponseAsync(InteractionResponseType.ChannelMessageWithSource, b)
            |> fun x -> x.GetAwaiter().GetResult()

        | false, _ -> ()

let componentInteractionCreateHandle (client: DiscordClient) (e: EventArgs.ComponentInteractionCreateEventArgs) =
    if e.Message.Author.Id = client.CurrentUser.Id then
        match e.Id with
        | AgeCreateModalButtonId ->
            let b = Entities.DiscordInteractionResponseBuilder()
                        .WithTitle("Modal User")
                        .WithCustomId(AgeModalId)
                        .AddComponents(
                            Entities.TextInputComponent(
                                "Возраст",
                                AgeInputComponentId,
                                required = true,
                                style = TextInputStyle.Short
                            )
                        )

            e.Interaction.CreateResponseAsync(InteractionResponseType.Modal, b).GetAwaiter().GetResult()

            true

        | AgeGetAgeStatisticsButtonId ->
            let b = Entities.DiscordInteractionResponseBuilder()
            b.IsEphemeral <- true

            if m.PostAndReply (fun r -> IsUserEnteredAge (r, e.User.Id)) then
                b.AddEmbed (createAgeStatisticEmbed e.Guild.Id) |> ignore
            else
                b.Content <- "Чтобы посмотреть статистику, сперва введите свой возраст."

            e.Interaction.CreateResponseAsync(InteractionResponseType.ChannelMessageWithSource, b).GetAwaiter().GetResult()

            true

        | _ -> false
    else
        false

let guildMemberRemoveHandle (e: EventArgs.GuildMemberRemoveEventArgs) =
    m.Post (RemoveGuildId (e.Member.Id, e.Guild.Id))
