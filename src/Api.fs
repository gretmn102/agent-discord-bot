module Api
open Shared

module ApiProtocol =
    open Newtonsoft.Json

    type Id = string

    type ErrorCode =
        | ParseError = -32700
        | InvalidRequest = -32600
        | MethodNotFound = -32601
        | InvalidParams = -32602
        | InternalError = -32603
        | ServerErrorStart = -32000
        | ServerErrorEnd = -32099

    type Error<'D> = {
        Code: ErrorCode
        Message: string
        Data: Option<'D>
    }
    with
        static member Create(code: ErrorCode, message: string, data): Error<'D> =
            { Code = code; Message = message; Data = data }

        static member ParseError data: Error<'D> =
            Error.Create(ErrorCode.ParseError, "Parse error", data)

        static member InvalidRequest data: Error<'D> =
            Error.Create(ErrorCode.InvalidRequest, "Invalid request", data)

        static member MethodNotFound data: Error<'D> =
            Error.Create(ErrorCode.MethodNotFound, "Method not found", data)

        static member InvalidParams data: Error<'D> =
            Error.Create(ErrorCode.InvalidParams, "Invalid params", data)

        static member InternalError data: Error<'D> =
            Error.Create(ErrorCode.InternalError, "Internal error", data)

    type Response<'R, 'ErrorData> = {
        Id: Id
        Result: Option<'R>
        Error: Option<Error<'ErrorData>>
    }
    with
        static member Create(id: Id, data: Result<'R, Error<'ErrorData>>): Response<'R, 'ErrorData> =
            match data with
            | Ok r ->
                {
                    Id = id
                    Result = Some r
                    Error = None
                }
            | Error err ->
                {
                    Id = id
                    Result = None
                    Error = Some err
                }

        member this.Serialize(): string =
            Api.Serializer.ser this

        static member Deserialize json: Result<Response<'R, 'ErrorData>, Response<'R, string>> =
            try
                Ok(Api.Serializer.des json)
            with e ->
                try
                    let t: Linq.JToken = Api.Serializer.des json
                    {
                        Id = t.Value "id" // can be null
                        Result = None
                        Error = Some (Error.ParseError(Some (sprintf "%A" e)))
                    }
                    |> Result.Error

                with e ->
                    {
                        Id = null
                        Result = None
                        Error = Some (Error.ParseError(Some e.Message))
                    }
                    |> Result.Error

    type Request<'D> = {
        Id: Option<Id>
        Data: 'D
    }
    with
        static member Create(id: Option<Id>, data: 'D): Request<'D> =
            {
                Id = id
                Data = data
            }

        member this.Serialize(): string =
            Api.Serializer.ser this

        static member Deserialize json: Result<Request<'D>, Response<unit, string>> =
            try
                Ok(Api.Serializer.des json)
            with e ->
                try
                    let t: Linq.JToken = Api.Serializer.des json
                    {
                        Id = t.Value "id" // can be null
                        Result = None
                        Error = Some (Error.ParseError(Some e.Message))
                    }
                    |> Result.Error

                with e ->
                    {
                        Id = null
                        Result = None
                        Error = Some (Error.ParseError(Some e.Message))
                    }
                    |> Result.Error

open IO.Ably
open IO.Ably.Realtime

type Request =
    | Ping
    | Doorkeeper of Doorkeeper.Api.Request

type Response =
    | Pong
    | Doorkeeper of Doorkeeper.Api.Response

let start (token: string) =
    let ably = new AblyRealtime(token)

    ably.Connection.On(ConnectionEvent.Connected, fun args ->
        printfn "Connected to Ably!\n%A" args
    )

    let reciveChannel = ably.Channels.Get("server")
    let replyChannel = ably.Channels.Get("bot")

    reciveChannel.Subscribe(fun message ->
        let publish (res: ApiProtocol.Response<_, _>) =
            replyChannel.Publish(
                "",
                res.Serialize(),
                (fun success errInfo ->
                    if not success then
                        printfn "Error:\n%A" errInfo),
                message.ClientId
            )

        match message.Data with
        | :? string as msg ->
            match ApiProtocol.Request.Deserialize msg with
            | Ok x ->
                match x.Data with
                | Ping ->
                    match x.Id with
                    | Some id ->
                        ApiProtocol.Response.Create(id, Ok Pong)
                        |> publish
                    | None -> ()

                | Request.Doorkeeper req ->
                    let res =
                        // TODO: Doorkeeper.Main.apiRequestHandle req
                        failwithf "TODO: Doorkeeper.Main.apiRequestHandle req"

                    match x.Id with
                    | Some id ->
                        ApiProtocol.Response.Create(id, Ok (Doorkeeper res))
                        |> publish
                    | None -> ()

            | Error err ->
                publish err
        | msg ->
            let msg = msg.ToString()

            ApiProtocol.Response.Create(null, Error (ApiProtocol.Error.ParseError (Some msg)))
            |> publish
    )

    // TODO: ably.Connection.On(ConnectionEvent.Disconnected, fun state -> disconnectedCallback state)
