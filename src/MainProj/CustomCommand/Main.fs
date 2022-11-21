module CustomCommand.Main
open FsharpMyExtension
open FsharpMyExtension.Either
open DSharpPlus

open Types
open Extensions

[<Struct>]
type Act =
    | CustomCommand of Model.Command

type Msg = Act * UserId option

module Parser =
    open FParsec

    open DiscordMessage.Parser

    type 'a Parser = Parser<'a, unit>

    let cmd: _ Parser =
        Model.commands
        |> Seq.collect (fun (KeyValue(commandId, x)) ->
            x.Names
            |> Array.map (fun x ->
                x, fun _ -> Model.commands.[commandId]
            )
        )
        |> Seq.sortByDescending (fun (commandName, _) -> commandName)
        |> Seq.map (fun (commandName, f) ->
            skipStringCI commandName |>> fun _ -> f ()
        )
        |> choice
        |>> CustomCommand

    let start f: _ Parser =
        cmd .>> spaces .>>. opt puserMention
        >>= fun msg ->
            preturn (fun x -> f x msg)

let r = System.Random ()

let reduce (client: DiscordClient) (e: EventArgs.MessageCreateEventArgs) ((msg, whomId): Msg) =
    match msg with
    | CustomCommand command ->
        let effect =
            let effects = command.RandomEffects
            effects.[r.Next(0, effects.Length)]

        let send whomId =
            let whom =
                whomId
                |> Option.map (fun whomId ->
                    try
                        await (client.GetUserAsync whomId)
                        |> Some
                        |> Ok
                    with e ->
                        sprintf "Пользователя %d не существует" whomId
                        |> Error
                )
                |> Option.defaultValue (Ok None)

            let b = Entities.DiscordEmbedBuilder()

            b.Color <- Entities.Optional.FromValue(DiscordEmbed.backgroundColorDarkTheme)

            match whom with
            | Ok whom ->
                let message =
                    match whom with
                    | None ->
                        effect.OnSelf
                    | Some whom ->
                        let authorId = e.Author.Id
                        let botId = client.CurrentUser.Id

                        if whom.Id = authorId then
                            effect.OnSelf
                        elif whom.Id = botId then
                            effect.OnBot
                        else
                            effect.OnOther

                let substitions =
                    MessageTemplate.Substitions.create
                        e.Author
                        client.CurrentUser
                        whom

                let createMessage rawMessage =
                    match MessageTemplate.Message.parse rawMessage with
                    | Ok message ->
                        MessageTemplate.Message.substitute substitions message
                    | Error errorMessage ->
                        errorMessage

                message.Embed.Description
                |> Option.iter (fun description ->
                    b.Description <- createMessage description
                )

                message.Embed.ImageUrl
                |> Option.iter (fun imageUrl ->
                    b.ImageUrl <- imageUrl
                )

            | Error errMsg ->
                b.Description <- errMsg

            awaiti (client.SendMessageAsync (e.Channel, b.Build()))

        match whomId with
        | Some whomId ->
            send (Some whomId)
        | None ->
            match e.Message.ReferencedMessage with
            | null ->
                send whomId
            | referencedMessage ->
                send (Some referencedMessage.Author.Id)

let exec: MessageCreateEventHandler Parser.Parser =
    Parser.start (fun (client: DiscordClient, e: EventArgs.MessageCreateEventArgs) msg ->
        reduce client e msg
    )
