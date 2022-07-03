module Server
open Saturn

let endpointPipe = pipeline {
    plug head
    plug requestId
}

let port =
    match System.Environment.GetEnvironmentVariable("PORT") with
    | null -> uint16 8087
    | port -> uint16 port

let app = application {
    pipe_through endpointPipe

    error_handler (fun ex _ -> pipeline { text ex.Message })
    use_router Router.appRouter
    url (sprintf "http://0.0.0.0:%d/" port)
    memory_cache
    use_static "static"
    use_gzip
}

[<EntryPoint>]
let main _ =
    printfn "Working directory - %s" (System.IO.Directory.GetCurrentDirectory())
    run app

    0
