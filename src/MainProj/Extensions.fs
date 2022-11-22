module Extensions
open FsharpMyExtension
open FsharpMyExtension.Either
open DSharpPlus

module DiscordEmbed =
    let backgroundColorDarkTheme = Entities.DiscordColor("#2f3136")

module Interaction =
    open Newtonsoft.Json

    type ComponentState<'ComponentId, 'Data when 'ComponentId: enum<int>> =
        {
            Id: string
            [<JsonProperty("CI")>]
            ComponentId: 'ComponentId
            [<JsonProperty("D")>]
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

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module ComponentState =
        let create id componentId data =
            {
                Id = id
                ComponentId = componentId
                Data = data
            }

        let header =
            "cid"

        module Printer =
            open FsharpMyExtension.ShowList

            let showEsapedString (str: string): ShowS =
                showString (str.Replace("\n", "\\\n"))

            let inline showEnum (enu: 'Enum) =
                shows (int enu)

            let inline showT (showData: _ -> ShowS) (x: ComponentState<'ComponentId, 'Data>): ShowS =
                showString header << nl
                << showEsapedString x.Id << nl
                << showEnum x.ComponentId << nl
                << showData x.Data

            let inline serialize (showData: _ -> ShowS) (x: ComponentState<'ComponentId, 'Data>): string =
                showT showData x
                |> show

        let inline serialize showData (x: ComponentState<'ComponentId, 'Data>) =
            Printer.serialize showData x

        module Parser =
            open FParsec

            type 'a Parser = Parser<'a, unit>

            let pheader: _ Parser =
                skipString header .>> newline

            let pescapedString: _ Parser =
                let pescapedNewline =
                    pchar '\\'
                    >>. (newlineReturn "\n" <|>% "\\")

                manyStrings
                    (many1Satisfy (isNoneOf "\\\n") <|> pescapedNewline)

            let inline parse pdata: ComponentState<'ComponentId, 'Data> Parser =
                let p =
                    pipe3
                        (pescapedString .>> newline)
                        (pint32 .>> newline)
                        pdata
                        (fun id componentId data ->
                            {
                                Id = id
                                ComponentId = (enum<'ComponentId> componentId)
                                Data = data
                            }
                        )

                pheader >>. p

        let inline tryDeserialize pdata str: Result<ComponentState<'ComponentId, 'Data>, _> option =
            match FParsecExt.runResult Parser.pheader str with
            | Ok _ ->
                FParsecExt.runResult (Parser.parse pdata) str
                |> Some
            | _ -> None

type DataOrUrl =
    | Data of string
    | Url of string
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module DataOrUrl =
    let getOrAttachment (message: Entities.DiscordMessage) (dataOrUrl: DataOrUrl option) =
        let download url =
            let res =
                WebDownloader.tryGet id url
                |> Async.RunSynchronously
            match res with
            | Right x ->
                match x.Content with
                | WebDownloader.Text json ->
                    Ok json
                | _ ->
                    Error (sprintf "Expected WebDownloader.Text but binary.")

            | Left x ->
                Error (sprintf "%A" x)

        match dataOrUrl with
        | Some json ->
            match json with
            | Url url ->
                download url
            | Data json ->
                Ok json

        | None ->
            match Seq.tryHead message.Attachments with
            | Some jsonFile ->
                // if jsonFile.MediaType = "application/json; charset=utf-8" then
                download jsonFile.Url
            | None ->
                Error "Нужно указать настройки либо явно, либо прикрепить файл к сообщению."

    module Parser =
        open FParsec

        open DiscordMessage.Parser

        type 'a Parser = Parser<'a, unit>

        let parser: _ Parser =
            let purl =
                skipString "http"
                >>. many1Satisfy ((<>) ' ')
                |>> sprintf "http%s"

            (purl |>> Url)
            <|> (pcodeBlock <|> many1Satisfy (fun _ -> true) |>> Data)
