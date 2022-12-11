module Age.Main
open FsharpMyExtension
open FsharpMyExtension.Either
open DSharpPlus

open Shared
open Types
open Extensions

type Request =
    | CreateForm

module Parser =
    open FParsec

    open DiscordMessage.Parser

    type 'Result Parser = Primitives.Parser<'Result, unit>

    let start f: _ Parser =
        pstring "ageCreateForm" >>% CreateForm
        >>= fun msg ->
            preturn (fun x -> f x msg)

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
        Users: Model.Users
    }

[<Struct>]
type AddOrRemove = Add | Remove

type Req =
    | Request of EventArgs.MessageCreateEventArgs * Request
    | InputAge of UserId * GuildId * int
    /// `age * count`
    | GetAgeStatistics of AsyncReplyChannel<Map<int, int>> * GuildId
    | IsUserEnteredAge of AsyncReplyChannel<bool> * UserId
    | AddOrRemoveGuildId of AddOrRemove * UserId * GuildId

let reduce (msg: Req) (state: State) =
    match msg with
    | InputAge (userId, guildId, age) ->
        let users =
            state.Users
            |> Model.Users.set userId (fun userData ->
                { userData with
                    Age = age
                    GuildIds = Set.add guildId userData.GuildIds
                }
            )

        { state with
            Users = users
        }

    | GetAgeStatistics (r, guildId) ->
        state.Users.Cache
        |> Map.fold
            (fun st userId user ->
                if Set.contains guildId user.Data.GuildIds then
                    st
                    |> Map.addOrModWith
                        user.Data.Age
                        (fun () -> 1)
                        (fun acc -> 1 + acc)
                else
                    st
            )
            Map.empty
        |> r.Reply

        state

    | IsUserEnteredAge(r, userId) ->
        Map.containsKey userId state.Users.Cache
        |> r.Reply

        state

    | AddOrRemoveGuildId(addOrRemove, userId, guildId) ->
        match Model.Users.tryFindById userId state.Users with
        | None -> state
        | Some userData ->
            let users =
                state.Users
                |> Model.Users.set userId (fun userData ->
                    { userData with
                        GuildIds =
                            match addOrRemove with
                            | Remove ->
                                Set.remove guildId userData.GuildIds
                            | Add ->
                                Set.add guildId userData.GuildIds
                    }
                )

            { state with
                Users = users
            }

    | Request(e, msg) ->
        match msg with
        | CreateForm ->
            awaiti <| e.Channel.TriggerTypingAsync()

            let b = Entities.DiscordMessageBuilder()

            b.Embed <-
                Entities.DiscordEmbedBuilder(
                    Color = Entities.Optional.FromValue(DiscordEmbed.backgroundColorDarkTheme),
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

let create collectionName (db: MongoDB.Driver.IMongoDatabase) =
    let m: MailboxProcessor<Req> =
        let init = {
            Users = Model.Users.init collectionName db
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

    let createAgeStatisticEmbed guildId =
        let ageStatistic = m.PostAndReply(fun r -> GetAgeStatistics(r, guildId))

        if Map.isEmpty ageStatistic then
            Entities.DiscordEmbedBuilder()
                .WithDescription("Пока что никто на этом сервере не прошел опрос.")
                .Build()
        else
            let ages, counts =
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

                awaiti <| interaction.CreateResponseAsync(InteractionResponseType.ChannelMessageWithSource, b)

            | false, _ -> ()

            true
        else
            false

    let componentInteractionCreateHandle ((client: DiscordClient), (e: EventArgs.ComponentInteractionCreateEventArgs)) =
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

                awaiti <| e.Interaction.CreateResponseAsync(InteractionResponseType.Modal, b)

                true

            | AgeGetAgeStatisticsButtonId ->
                let b = Entities.DiscordInteractionResponseBuilder()
                b.IsEphemeral <- true

                if m.PostAndReply (fun r -> IsUserEnteredAge (r, e.User.Id)) then
                    b.AddEmbed (createAgeStatisticEmbed e.Guild.Id) |> ignore
                else
                    b.Content <- "Чтобы посмотреть статистику, сперва введите свой возраст."

                awaiti <| e.Interaction.CreateResponseAsync(InteractionResponseType.ChannelMessageWithSource, b)

                true

            | _ -> false
        else
            false

    { BotModule.empty with
        MessageCreateEventHandleExclude =
            let exec: MessageCreateEventHandler Parser.Parser =
                Parser.start (fun (client: DiscordClient, e: EventArgs.MessageCreateEventArgs) msg ->
                    m.Post (Request (e, msg))
                )

            Some exec

        GuildMemberAddedHandler =
            let guildMemberAddedHandle (e: EventArgs.GuildMemberAddEventArgs) =
                m.Post (AddOrRemoveGuildId (Add, e.Member.Id, e.Guild.Id))
            Some guildMemberAddedHandle

        GuildMemberRemovedHandler =
            let guildMemberRemoveHandle (e: EventArgs.GuildMemberRemoveEventArgs) =
                m.Post (AddOrRemoveGuildId (Remove, e.Member.Id, e.Guild.Id))
            Some guildMemberRemoveHandle

        ComponentInteractionCreateHandle =
            Some componentInteractionCreateHandle

        ModalSubmit =
            Some modalHandle
    }
