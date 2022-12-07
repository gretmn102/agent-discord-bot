module CustomCommand.Main
open FsharpMyExtension
open FsharpMyExtension.Either
open DSharpPlus

open Types
open Extensions

type CommandManagementRequest =
    | Set of json: DataOrUrl option
    | Print

type CommandRequest = Model.CommandT * UserId option

type Request =
    | CommandManagementRequest of CommandManagementRequest
    | CommandRequest of CommandRequest

type Handler =
    | NewMessageHandle of client: DiscordClient * e: EventArgs.MessageCreateEventArgs

type CommandParser = FParsec.Primitives.Parser<CommandRequest, unit>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module CommandsParser =
    open FParsec

    open DiscordMessage.Parser

    type 'a Parser = Parser<'a, unit>

    let create (commands: Map<Model.CommandId, Model.CommandT>) =
        let commandParser =
            commands
            |> Seq.collect (fun (KeyValue(commandId, x)) ->
                x.Data.Names
                |> Array.map (fun x ->
                    x, fun _ -> commands.[commandId]
                )
            )
            |> Seq.sortByDescending (fun (commandName, _) -> commandName)
            |> Seq.map (fun (commandName, f) ->
                skipStringCI commandName |>> fun _ -> f ()
            )
            |> choice

        commandParser .>> spaces .>>. opt puserMention

module CommandManagementParser =
    open FParsec

    open DiscordMessage.Parser

    type 'a Parser = CommandsParser.Parser<'a>

    let pset: _ Parser =
        skipStringCI "setCustomCommands" .>> spaces
        >>. opt DataOrUrl.Parser.parser

    let pprintCommand: _ Parser =
        skipStringCI "команды"

    let parser: _ Parser =
        choice [
            pset |>> CommandManagementRequest.Set
            pprintCommand >>% CommandManagementRequest.Print
        ]

module Parser =
    open FParsec

    open DiscordMessage.Parser

    type 'a Parser = CommandsParser.Parser<'a>

    let prefix: _ Parser = pchar '.'

    let create (commandsParser: CommandParser): _ Parser =
        let p =
            choice [
                commandsParser |>> CommandRequest
                CommandManagementParser.parser |>> CommandManagementRequest
            ]

        (prefix >>? p |>> Some) <|>% None

let r = System.Random ()

type State =
    {
        Commands: Model.Commands
        CommandsParser: CommandParser
    }

let reduce (req: Handler) (state: State) =
    match req with
    | NewMessageHandle(client, e) ->
        match FParsecExt.runResult (Parser.create state.CommandsParser) e.Message.Content with
        | Ok reqOpt ->
            match reqOpt with
            | Some req ->
                match req with
                | CommandRequest(command, whomId) ->
                    let effect =
                        let effects = command.Data.RandomEffects
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

                    state

                | CommandManagementRequest req ->
                    awaiti <| e.Channel.TriggerTypingAsync()

                    let send msg =
                        let embed = Entities.DiscordEmbedBuilder()
                        embed.Color <- Entities.Optional.FromValue(DiscordEmbed.backgroundColorDarkTheme)
                        embed.Description <- msg

                        let b = Entities.DiscordMessageBuilder()
                        b.Embed <- embed.Build()

                        awaiti <| e.Channel.SendMessageAsync b

                    let isBotOwner (user: Entities.DiscordUser) next =
                        if user.Id = Db.superUserId then
                            next ()
                        else
                            send (sprintf "Только <@%d> может пользоваться этой командой." Db.superUserId)
                            state

                    match req with
                    | CommandManagementRequest.Set json ->
                        let deserializeSettings json next =
                            match Model.CommandsArray.tryDeserialize json with
                            | Ok itemsArray ->
                                next itemsArray
                            | Error errMsg ->
                                send (sprintf "%A" errMsg)
                                state

                        let getJson json next =
                            match DataOrUrl.getOrAttachment e.Message json with
                            | Ok x -> next x
                            | Error errMsg ->
                                send errMsg
                                state

                        isBotOwner e.Author <| fun () ->
                        getJson json <| fun json ->
                        deserializeSettings json <| fun commands ->

                        let state =
                            { state with
                                Commands =
                                    Model.Commands.sets commands state.Commands
                                CommandsParser =
                                    CommandsParser.create (commands |> Array.map (fun x -> x.Id, x) |> Map.ofArray)
                            }

                        send "Done!"

                        state

                    | CommandManagementRequest.Print ->
                        let commands =
                            state.Commands.Cache
                            |> Seq.map (fun (KeyValue(id, v)) ->
                                v.Data.Names
                                |> Array.map (sprintf "`%s`")
                                |> String.concat ", "
                                |> sprintf "• %s"
                            )
                            |> String.concat "\n"

                        send commands

                        state

            | None ->
                state

        | Error errMsg ->
            let send msg =
                let embed = Entities.DiscordEmbedBuilder()
                embed.Color <- Entities.Optional.FromValue(DiscordEmbed.backgroundColorDarkTheme)
                embed.Description <- msg

                let b = Entities.DiscordMessageBuilder()
                b.Embed <- embed.Build()

                awaiti <| e.Channel.SendMessageAsync b

            send errMsg

            state

let m =
    let commands = Model.Commands.init "customCommands" Db.database
    let init = {
        Commands = commands
        CommandsParser = CommandsParser.create commands.Cache
    }

    MailboxProcessor.Start (fun mail ->
        let rec loop (state: State) =
            async {
                let! req = mail.Receive()
                let state =
                    try
                        reduce req state
                    with e ->
                        printfn "%A" e
                        state

                return! loop state
            }
        loop init
    )

let handle client e =
    m.Post (NewMessageHandle (client, e))
