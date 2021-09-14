module FParsecUtils
open FParsec
open FsharpMyExtension
open FsharpMyExtension.Either

let runEither p str =
    match run p str with
    | Success(x, _, _) -> Right x
    | Failure(x, _, _) -> Left x

let pbigint<'UserState> : Primitives.Parser<_, 'UserState> =
    let digitsToNum xs =
        xs
        |> List.rev
        |> List.fold (fun (acc, i) x ->
            (acc + x * pown 10I i, i + 1)
             ) (0I, 0)
        |> fst
    many1 digit |>> (List.map (fun d -> bigint (int d - 48)) >> digitsToNum)
