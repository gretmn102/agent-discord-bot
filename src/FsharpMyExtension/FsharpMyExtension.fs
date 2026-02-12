[<AutoOpen>]
module FsharpMyExtension

let flip f x y = f y x

module Map =
    /// Add or update: if key exists, apply update to value; else add key with init().
    let addOrModWith (key: 'k) (init: unit -> 'v) (update: 'v -> 'v) (m: Map<'k, 'v>) : Map<'k, 'v> =
        match Map.tryFind key m with
        | None -> Map.add key (init ()) m
        | Some v -> Map.add key (update v) m

module Either =
    type Either<'L, 'R> =
        | Left of 'L
        | Right of 'R

    let map (f: 'r -> 'r2) (e: Either<'l, 'r>) : Either<'l, 'r2> =
        match e with
        | Left x -> Left x
        | Right x -> Right(f x)

    let iter (f: 'r -> unit) (e: Either<'l, 'r>) : unit =
        match e with
        | Left _ -> ()
        | Right x -> f x

    let get (e: Either<'l, 'r>) : 'r =
        match e with
        | Left x -> failwithf "Either.get: expected Right, got Left(%A)" x
        | Right x -> x

module ResultExt =
    /// Common Result helpers. Ok/Error are from FSharp.Core.
    let map (f: 'a -> 'b) (r: Result<'a, 'e>) : Result<'b, 'e> =
        match r with
        | Ok x -> Ok(f x)
        | Error e -> Error e

    let bind (f: 'a -> Result<'b, 'e>) (r: Result<'a, 'e>) : Result<'b, 'e> =
        match r with
        | Ok x -> f x
        | Error e -> Error e

module ListZipper =
    type ListZ<'a> = ZZ of ('a list * 'a * 'a list)

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module ListZ =
        let ofList (xs: 'a list) : ListZ<'a> =
            match xs with
            | [] -> failwith "ListZipper.ListZ.ofList: empty list"
            | x :: xs -> ZZ([], x, xs)

        let next (ZZ(l, c, r)) : ListZ<'a> option =
            match r with
            | [] -> None
            | x :: xs -> Some(ZZ(c :: l, x, xs))

        let hole (ZZ(_, c, _)) : 'a = c
