module Types

type UserId = uint64
type MessageId = uint64

open System.Threading.Tasks

let await (t:Task<_>) =
    t.GetAwaiter() |> fun x -> x.GetResult()
let awaiti (t:Task<_>) =
    await t |> ignore