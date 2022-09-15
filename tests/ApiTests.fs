module ApiTests
open Fuchu
open IO.Ably
open IO.Ably.Realtime

open Types
open Shared.Api

[<Tests>]
let snowflakeTests =
    testList "snowflakeTests" [
        testCase "serialize and deserialize" <| fun () ->
            let converter = SnowflakeConverter()

            let ser x =
                Newtonsoft.Json.JsonConvert.SerializeObject(x, converter)

            let des (str: string) =
                Newtonsoft.Json.JsonConvert.DeserializeObject(str, converter)

            let sample = Snowflake.Create 12345UL

            let exp = "\"12345\""
            let act = ser sample

            Assert.Equal("", exp, act)

            let act: Snowflake = des exp

            Assert.Equal("", sample, act)
    ]

module Doorkeeper =
    module ApiTests =
        open Doorkeeper.Api.TransferTypes

        [<Tests>]
        let doorkeeperApiSerializeTests =
            testList "doorkeeperApiSerializeTests" [
                testCase "undefined NewcomerWelcomeMessageLog" <| fun () ->
                    let exp =
                        {
                            Channel = Some (EnabledOptionValue.Init (Snowflake.Create 42UL))
                            NewcomerWelcomeMessage = Some EnabledOptionValue.Empty
                            NewcomerWelcomeMessageLog = None
                        }

                    let json =
                        """{"channel":{"isEnabled":true,"value":"42"},"newcomerWelcomeMessage":{"isEnabled":false,"value":null}}"""

                    let act = Serializer.des json

                    Assert.Equal("", exp, act)
            ]
        ()

module ApiProtocolTests =
    [<Tests>]
    let apiProtocolSerializeTests =
        testList "apiProtocolSerialize" [
            testCase "Request and Response serialization" <| fun () ->
                let id = "42"
                let exp = Api.ApiProtocol.Request.Create(Some id, Api.Ping)
                let req = exp.Serialize()

                let act = Api.ApiProtocol.Request.Deserialize req
                Assert.Equal("", Ok exp, act)

                let exp = Api.ApiProtocol.Response.Create(id, Ok Api.Pong)
                let req = exp.Serialize()

                let act = Api.ApiProtocol.Response.Deserialize req
                Assert.Equal("", Ok exp, act)

            testCase "request option test and camelCase" <| fun () ->
                let sample =
                    let id = "42"

                    let setting: Doorkeeper.Api.TransferTypes.MainData =
                        {
                            Checkpoint = None
                            Inner = Some {
                                Channel = Some <| EnabledOptionValue.Init (Snowflake.Create 0UL)
                                NewcomerWelcomeMessage = None
                                NewcomerWelcomeMessageLog = Some <| EnabledOptionValue.Empty
                            }
                            Exit = None
                            Log = None
                        }

                    let x =
                        Api.Response.Doorkeeper (Doorkeeper.Api.Response.Get (Some setting))

                    Api.ApiProtocol.Request.Create(Some id, x)

                let act = sample.Serialize()

                let exp = """{"id":"42","data":{"case":"Doorkeeper","fields":[{"case":"Get","fields":[{"checkpoint":null,"inner":{"channel":{"isEnabled":true,"value":"0"},"newcomerWelcomeMessage":null,"newcomerWelcomeMessageLog":{"isEnabled":false,"value":null}},"exit":null,"log":null}]}]}}"""

                Assert.Equal("", exp, act)

                let json = """{"id":"42","data":{"case":"Doorkeeper","fields":[{"case":"Get","fields":[{"inner":{"channel":{"isEnabled":true,"value":"0"},"newcomerWelcomeMessageLog":{"isEnabled":false,"value":null}}}]}]}}"""
                let act = Api.ApiProtocol.Request.Deserialize json

                Assert.Equal("", Ok sample, act)

            testCase "response option test and camelCase" <| fun () ->
                let sample =
                    let id = "42"

                    Api.ApiProtocol.Response.Create(id, Ok Api.Ping)

                let act = sample.Serialize()

                let exp = """{"id":"42","result":{"case":"Ping"},"error":null}"""

                Assert.Equal("", exp, act)

                let act = Api.ApiProtocol.Response.Deserialize exp

                Assert.Equal("", Ok sample, act)
        ]

open Api

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

            let exp = """{"id":"42","result":{"case":"Pong"},"error":null}"""
            Assert.Equal("", exp, act)
    ]
