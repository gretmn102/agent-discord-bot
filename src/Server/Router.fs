module Router
open Saturn
open Giraffe.Core

let someScopeOrController = router {
    get "/" (json "hello world")
    get "/index.html" (redirectTo false "/")
    get "/default.html" (redirectTo false "/")
}

let api = pipeline {
    // plug acceptJson
    set_header "x-pipeline-type" "Api"
}

let apiRouter = router {
    not_found_handler (text "Api 404")
    pipe_through api

    forward "" someScopeOrController
}

let appRouter = router {
    forward "" apiRouter
}

