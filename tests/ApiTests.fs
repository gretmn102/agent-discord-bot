module ApiTests
open Fuchu
open IO.Ably
open IO.Ably.Realtime

open Types
open Api

module ApiProtocolTests =
    [<Tests>]
    let ApiProtocolTests =
        testList "ApiProtocolTests" [
            testCase "Request and Response serialization" <| fun () ->
                let id = "42"
                let exp = ApiProtocol.Request.Create(Some id, Ping)
                let req = exp.Serialize()

                let act = ApiProtocol.Request.Deserialize req
                Assert.Equal("", Ok exp, act)

                let exp = ApiProtocol.Response.Create(id, Ok Pong)
                let req = exp.Serialize()

                let act = ApiProtocol.Response.Deserialize req
                Assert.Equal("", Ok exp, act)
        ]

module MailMutex =
    type Msg<'S> =
        | SetState of 'S
        | GetState of AsyncReplyChannel<'S>

    let start initState =
        MailboxProcessor.Start(fun m ->
            let rec loop state =
                async {
                    let! msg = m.Receive()

                    match msg with
                    | SetState counter ->
                        return! loop counter

                    | GetState r ->
                        r.Reply state

                        return! loop state
                }
            loop initState
        )

    let setState state (m: MailboxProcessor<Msg<'S>>) = m.Post(SetState state)

    let getState (m: MailboxProcessor<Msg<'S>>) = m.PostAndReply GetState

[<Tests>]
let pingTests =
    testList "pingTests" [
        testCase "base" <| fun () ->
            let token =
                let tokenVarName = "AblyToken"
                getEnvironmentVariable tokenVarName
                |> Option.defaultWith (fun () -> failwithf "'%s' not set" tokenVarName)

            Api.start token

            let ably = new AblyRealtime(token)

            let replyChannel = ably.Channels.Get("server")
            let reciveChannel = ably.Channels.Get("bot")

            let id = "42"

            ably.Connection.On(ConnectionEvent.Connected, fun args ->
                let publish (req: ApiProtocol.Request<_>) =
                    let reqStr = req.Serialize()

                    replyChannel.Publish(
                        "",
                        reqStr,
                        (fun success errInfo ->
                            if not success then
                                printfn "Error:\n%A" errInfo)
                    )

                let req = ApiProtocol.Request.Create (Some id, Ping)

                publish req
            )

            let m = MailMutex.start None

            reciveChannel.Subscribe(fun message ->
                MailMutex.setState (Some (message.Data.ToString ())) m
            )

            let rec getState () =
                match MailMutex.getState m with
                | None ->
                    System.Threading.Thread.Sleep 100
                    getState ()
                | Some state -> state

            let act = getState ()

            let exp = """{"Id":"42","Result":{"Case":"Some","Fields":[{"Case":"Pong"}]},"Error":null}"""
            Assert.Equal("", exp, act)
    ]
