module Scheduler

type JobId = System.DateTime

type Job<'JobType> =
    {
        Time: System.DateTime
        Type: 'JobType
    }

type State<'JobType> =
    {
        Jobs: Map<JobId, Job<'JobType>>
    }
    static member Empty =
        {
            Jobs = (Map.empty: Map<JobId, Job<'JobType>>)
        }

type Req<'JobType> =
    | AddJob of Job<'JobType>
    | RemoveJob of JobId
    | PeekJob
    | GetJob of JobId

type Resp<'JobType> =
    | Done
    | EarlyJob of Job<'JobType> option
    | TakenJob of Job<'JobType> option

let reduce (cmd: 'JobType Req) (state: 'JobType State) =
    match cmd with
    | PeekJob ->
        let firstJob = Seq.tryHead state.Jobs |> Option.map (fun x -> x.Value)
        EarlyJob firstJob, state
    | AddJob job ->
        let state =
            { state with
                Jobs = Map.add job.Time job state.Jobs // TODO: What if it overwrites an existing job?
            }
        Done, state
    | GetJob job ->
        let res = Map.tryFind job state.Jobs
        TakenJob res, state
    | RemoveJob job ->
        let state =
            { state with
                Jobs = Map.remove job state.Jobs
            }
        Done, state


type Scheduler<'JobType>(state) =
    let m =
        MailboxProcessor.Start(fun mail ->
            let rec f st =
                async {
                    let! (reply: AsyncReplyChannel<_>, msg) = mail.Receive()
                    let (rep: Resp<'JobType>), state = reduce msg st
                    reply.Reply rep

                    return! f state
                }
            f state
        )

    member __.AddJob job =
        m.PostAndReply(fun r -> r, AddJob job)
        |> ignore

    member __.RemoveJob jobId =
        m.PostAndReply(fun r -> r, RemoveJob jobId)
        |> ignore

    member __.PeekJob () =
        match m.PostAndReply(fun r -> r, PeekJob) with
        | EarlyJob x -> x
        | x -> failwithf "Internal error: expected EarlyJob in PeekJob but %A" x

    member __.GetJob jobId =
        match m.PostAndReply(fun r -> r, GetJob jobId) with
        | TakenJob x -> x
        | x -> failwithf "Internal error: expected TakenJob in GetJob but %A" x

let startAsync (scheduler: Scheduler<'JobType>) (msTimeout: int) exec =
    let cancelToken = ref false

    async {
        while not !cancelToken do
            match scheduler.PeekJob () with
            | Some job ->
                if System.DateTime.Now >= job.Time then
                    exec job

                    ignore <| scheduler.RemoveJob job.Time

            | None -> ()

            System.Threading.Thread.Sleep msTimeout
    }
    |> Async.Start

    cancelToken
