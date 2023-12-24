module BallotBox.Core
open FsharpMyExtension
open FsharpMyExtension.Either
open DiscordBotExtensions.Types

type ChoiceId = int
type Choice =
    {
        Id: ChoiceId
        Description: string
        Score: int
    }
type State =
    {
        Description: string
        Choices: Map<ChoiceId, Choice>
        VotedUsers: Map<UserId, ChoiceId>
        IsPublic: bool
        OwnerId: UserId
    }

type Msg =
    | Vote of UserId * ChoiceId
    | MakePublic of UserId

let init ownerId description descriptionChoices =
    {
        Description = description
        Choices =
            descriptionChoices
            |> List.mapi (fun i x ->
                let choice =
                    {
                        Id = i
                        Description = x
                        Score = 0
                    }
                i, choice
            )
            |> Map.ofList
        VotedUsers = Map.empty
        IsPublic = false
        OwnerId = ownerId
    }

type Response =
    | YouAlreadyVoted
    | CanOnlyBeMadePublicByOwner
type ResultUpdate = (bool * State) * Response option

let update (msg:Msg) (state:State) : ResultUpdate =
    match msg with
    | Vote (userId, choiceId) ->
        match Map.tryFind userId state.VotedUsers with
        | None ->
            let state =
                { state with
                    VotedUsers = Map.add userId choiceId state.VotedUsers
                    Choices =
                        let choices = state.Choices
                        let choice = choices.[choiceId]
                        Map.add choiceId { choice with Score = choice.Score + 1 } choices
                }
            (true, state), None
        | Some(value) ->
            (false, state), Some YouAlreadyVoted
    | MakePublic userId ->
        if userId = state.OwnerId then
            let state =
                { state with
                    IsPublic = true
                }
            (true, state), None
        else
            (false, state), Some CanOnlyBeMadePublicByOwner

[<Literal>]
let SelectMenuId = "selectMenuId"

[<Literal>]
let MakePublicId = "makePublicId"

let view (((isUpdateState, state), response):ResultUpdate): ResultView =
    {
        View =
            if isUpdateState then
                let b = DSharpPlus.Entities.DiscordMessageBuilder()
                let embed = DSharpPlus.Entities.DiscordEmbedBuilder()

                let printResult () =
                    state.Choices
                    |> Seq.sortBy (fun (KeyValue(_, v)) -> -v.Score, v.Description)
                    |> Seq.fold
                        (fun (st1, st2) (KeyValue(_, v)) ->
                            v.Description::st1, string v.Score::st2
                        )
                        ([], [])
                    |> fun (xs, ys) ->
                        let f = List.rev >> String.concat "\n"
                        embed.AddField("Пункты", f xs, true)
                             .AddField("Голоса", f ys, true)
                        |> ignore

                if state.IsPublic then
                    embed.Description <-
                        // let choices =
                        //     state.Choices
                        //     |> Seq.sortBy (fun (KeyValue(_, v)) -> -v.Score, v.Description)
                        //     |> Seq.map (fun (KeyValue(_, v)) ->
                        //         sprintf "%s — %d" v.Description v.Score
                        //     )
                        //     |> String.concat "\n"
                        sprintf "%s\nПроголосовало %d человек" state.Description state.VotedUsers.Count
                    printResult ()
                else
                    embed.Description <-
                        sprintf "%s\nПроголосовало %d человек" state.Description state.VotedUsers.Count
                    let options =
                        state.Choices
                        |> Seq.map (fun (KeyValue(k, v)) ->
                            DSharpPlus.Entities.DiscordSelectComponentOption(v.Description, string k)
                        )

                    DSharpPlus.Entities.DiscordSelectComponent(SelectMenuId, "select", options)
                    |> b.AddComponents |> ignore

                    DSharpPlus.Entities.DiscordButtonComponent(DSharpPlus.ButtonStyle.Danger, MakePublicId, "Опубликовать (может только владелец опросника)")
                    |> b.AddComponents |> ignore

                b.Embed <- embed.Build ()
                Some b
            else
                None

        ResponseToUser =
            response
            |> Option.map (fun x ->
                let b = DSharpPlus.Entities.DiscordMessageBuilder()
                b.Content <-
                    match x with
                    | YouAlreadyVoted ->
                        "Ты уже проголосовал"
                    | CanOnlyBeMadePublicByOwner ->
                        "Опубликовать итоги опроса может только владелец"
                b
            )
    }

let update2 (e:DSharpPlus.EventArgs.ComponentInteractionCreateEventArgs) state =
    let msg =
        match e.Id with
        | SelectMenuId ->
            Vote (e.User.Id, int e.Values.[0])
        | MakePublicId ->
            MakePublic e.User.Id
        | x -> failwithf "expected SelectMenuId or MakePublicId but %s" x
    let (((_, state), _) as res) = update msg state
    view res, state

let init2 ownerId description descriptionChoices =
    let state = init ownerId description descriptionChoices
    view ((true, state), None), state
