module Scheduler

type JobId = System.DateTime

type JobType =
    | MakeCoffee
    | CongratulateWithCelebration

type Job =
    {
        Time: System.DateTime
        Type: JobType
    }

type State =
    {
        Jobs: Map<JobId, Job>
    }
    static member Empty =
        {
            Jobs = Map.empty
        }

type Req =
    | AddJob of Job
    | RemoveJob of JobId
    | PeekJob

type Resp =
    | Done
    | EarlyJob of Job option

let reduce (cmd: Req) (state: State) =
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
    | RemoveJob job ->
        let state =
            { state with
                Jobs = Map.remove job state.Jobs
            }
        Done, state

let tests () =
    let state = State.Empty

    let firstJob = { Time = System.DateTime.Now; Type = MakeCoffee }

    let _, state =
        reduce (AddJob firstJob) state
    let _, state =
        reduce (AddJob { Time = System.DateTime.Now.AddDays 1.; Type = CongratulateWithCelebration }) state

    let res, _ = reduce PeekJob state
    match res with
    | EarlyJob(Some job) -> job = firstJob
    | x -> failwithf "expected EarlyJob(Some job) but %A" x

// tests ()

let m =
    MailboxProcessor.Start(fun mail ->
        let rec f st =
            async {
                let! (reply: AsyncReplyChannel<_>, msg) = mail.Receive()
                let rep, state = reduce msg st
                reply.Reply rep

                return! f state
            }
        f State.Empty
    )

let addJob job =
    m.PostAndReply(fun r -> r, AddJob job)

let removeJob jobId =
    m.PostAndReply(fun r -> r, RemoveJob jobId)

let peekJob () =
    m.PostAndReply(fun r -> r, PeekJob)

let startAsync (msTimeout: int) =
    let cancelToken = ref false

    async {
        while not !cancelToken do
            match peekJob () with
            | EarlyJob job ->
                match job with
                | Some job ->
                    if System.DateTime.Now >= job.Time then
                        match job.Type with
                        | MakeCoffee ->
                            printfn "MakeCoffee!"
                        | CongratulateWithCelebration ->
                            printfn "Congratulation!"

                        ignore <| removeJob job.Time

                | None -> ()
            | x -> failwithf "%A" x

            System.Threading.Thread.Sleep msTimeout
    }
    |> Async.Start

    cancelToken

// let cancelToken = startAsync 200
// addJob { Time = System.DateTime.Now.AddMinutes 2.; Type = MakeCoffee }
