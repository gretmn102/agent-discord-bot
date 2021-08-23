module Quiz
open FsharpMyExtension

type Cost = int
type Question = { Description:string; Variants:(string * Cost) [] }
type Result = { Range:int*int; Description:string }

type Question2 = { Description:string; Variants:string [] }

type QuizType = Question list * Result list

module Core =
    type T =
        /// `Question2 * (n:int -> (unit -> T) option)`, where `n` is `[0..]`
        | Req of Question2 * (int -> (unit -> T) option)
        | End of string

    type State = { Score : int; Questions:Question list }
    let exec (results:Result list) st =
        let rec loop (st:State) =
            match st.Questions with
            | q::qs ->
                let q' = { Description = q.Description; Variants = q.Variants |> Array.map fst }
                Req(q', fun i ->
                    let vars = q.Variants
                    if 0 <= i && i < Array.length vars then
                        fun () ->
                            {
                                Score = st.Score + snd vars.[i]
                                Questions = qs
                            }
                            |> loop
                        |> Some
                    else None
                )
            | [] ->
                let score = st.Score
                let res =
                    results
                    |> List.tryFind (fun x ->
                        let from, to' = x.Range
                        from <= score && score <= to')
                match res with
                | Some x -> End x.Description
                | None -> failwith "res"
        loop st

type Msg =
    | Start
    | Select of int

type State = { Now: Core.T; Quiz: QuizType }

let init (quiz:QuizType) =
    {
        Now =
            let questions, res = quiz
            {
                Core.Score = 0
                Core.Questions = questions
            }
            |> Core.exec res
        Quiz = quiz
    }

let update (msg:Msg) (state:State) =
    match msg with
    | Select num ->
        match state.Now with
        | Core.T.Req(_, f) ->
            match f num with
            | Some f ->
                { state with Now = f() }
            | None -> failwith "Введите цифру указанного ответа."
        | Core.T.End _ ->
            failwith "ERROR: `Abstract.T.End _`"
    | Start ->
        init state.Quiz

[<Literal>]
let SelectMenuId = "selectMenuId"

let view (st:State) =
    match st.Now with
    | Core.T.Req(x, f) ->
        let b = DSharpPlus.Entities.DiscordMessageBuilder()
        let embed = DSharpPlus.Entities.DiscordEmbedBuilder()
        embed.Description <- x.Description
        b.Embed <- embed.Build ()

        let options =
            x.Variants
            |> Array.mapi (fun i label ->
                DSharpPlus.Entities.DiscordSelectComponentOption(label, string i)
            )

        let c = DSharpPlus.Entities.DiscordSelectComponent(SelectMenuId, "select", options)
        b.AddComponents c |> ignore

        b
    | Core.T.End resultMessage ->
        let b = DSharpPlus.Entities.DiscordMessageBuilder()
        let embed = DSharpPlus.Entities.DiscordEmbedBuilder()
        embed.Description <- resultMessage
        b.Embed <- embed.Build ()

        b

open FsharpMyExtension.Either
open System.Threading.Tasks

let loadQuiz () : QuizType =
    let path = "quiz.json"
    if System.IO.File.Exists path then
        try
            let x:QuizType = Json.desf path
            x
        with e ->
            failwithf "Error '%s'\n%s" path e.Message
    else failwithf "Not found '%s'" path
