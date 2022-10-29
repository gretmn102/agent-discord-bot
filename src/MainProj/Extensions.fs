module Extensions

module DiscordEmbed =
    let backgroundColorDarkTheme = DSharpPlus.Entities.DiscordColor("#2f3136")

module Interaction =
    open FsharpMyExtension
    open Newtonsoft.Json

    type ComponentState<'ComponentId, 'Data> =
        {
            Id: string
            ComponentId: 'ComponentId
            Data: 'Data
        }
        static member Serialize (x: ComponentState<'ComponentId, 'Data>) =
            Json.serNotIndent x

        static member Deserialize (s: string): Result<ComponentState<'ComponentId, 'Data>, string> =
            try
                Ok(Json.des s)
            with e ->
                Error(e.Message)

        static member TryDeserialize (id: string) (rawJson: string) =
            try
                JsonConvert.DeserializeObject<Linq.JObject> rawJson
                |> Some
            with e ->
                None
            |> Option.bind (fun jobject ->
                match jobject.TryGetValue "Id" with
                | true, v ->
                    if v.Type = Linq.JTokenType.String then
                        let currentId = (v :?> Linq.JValue).Value :?> string
                        if currentId = id then
                            try
                                jobject.ToObject<ComponentState<'ComponentId, 'Data>>()
                                |> Ok
                            with e ->
                                Error e.Message
                            |> Some
                        else
                            None
                    else
                        None
                | false, _ -> None
            )
