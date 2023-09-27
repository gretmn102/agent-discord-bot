module CustomCommand.Main
open FsharpMyExtension
open FsharpMyExtension.Either
open DSharpPlus
open DiscordBotExtensions
open DiscordBotExtensions.Types
open DiscordBotExtensions.Extensions

open CustomCommand

type CommandManagementRequest =
    | Set of json: DataOrUrl option
    | Print

type CommandRequest = Model.Command * UserId option

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

    let create (commands: Map<Model.CommandId, Model.Command>) =
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

type State =
    {
        Commands: Model.Commands
        CommandsParser: CommandParser
        UserCooldownsStorage: Model.UserCooldownsStorage
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
                    let send whomId =
                        let whom =
                            let getDiscordUser userId =
                                try
                                    await (client.GetUserAsync userId)
                                    |> Ok
                                with e ->
                                    sprintf "Пользователя %d не существует" userId
                                    |> Error

                            match whomId with
                            | Some whomId ->
                                getDiscordUser whomId |> Result.map Some
                            | None ->
                                Ok None

                        let updateCooldownOpt, b =
                            match whom with
                            | Ok whom ->
                                let authorId = e.Author.Id

                                let getReactions (commandData: Model.CommandData) =
                                    commandData
                                    |> Model.CommandData.getReactions
                                        (fun () -> authorId)
                                        (fun () -> client.CurrentUser.Id)
                                        whomId

                                let getCooldownReactions (cooldownable: Model.Cooldownable) =
                                    cooldownable
                                    |> Model.Cooldownable.getReactions
                                        (fun () -> authorId)
                                        (fun () -> client.CurrentUser.Id)
                                        whomId

                                let updateCooldownOpt, reactions =
                                    match command.Data.Cooldownable with
                                    | None ->
                                        None, getReactions command.Data

                                    | Some cooldownable ->
                                        let userCooldownId =
                                            Model.UserCooldownId.create authorId command.Id

                                        let userCooldown =
                                            Model.UserCooldownsStorage.tryFindById
                                                userCooldownId
                                                state.UserCooldownsStorage

                                        match userCooldown with
                                        | None ->
                                            None, getReactions command.Data

                                        | Some userCooldown ->
                                            if cooldownable.Cooldown - userCooldown.Data.LastDateTimeActivated.Ticks > 0L then
                                                None, getCooldownReactions cooldownable
                                            else
                                                Some userCooldownId, getReactions command.Data

                                let reaction =
                                    Model.ReactionsList.randomGet reactions

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

                                let b = Entities.DiscordEmbedBuilder()

                                b.Color <- Entities.Optional.FromValue(DiscordEmbed.backgroundColorDarkTheme)

                                let message = reaction.Message
                                message.Embed.Description
                                |> Option.iter (fun description ->
                                    b.Description <- createMessage description
                                )

                                message.Embed.ImageUrl
                                |> Option.iter (fun imageUrl ->
                                    b.ImageUrl <- imageUrl
                                )

                                updateCooldownOpt, b

                            | Error errMsg ->
                                let b = Entities.DiscordEmbedBuilder()
                                b.Color <- Entities.Optional.FromValue(DiscordEmbed.backgroundColorDarkTheme)
                                b.Description <- errMsg
                                None, b

                        awaiti (client.SendMessageAsync (e.Channel, b.Build()))

                        match updateCooldownOpt with
                        | None ->
                            state
                        | Some userCooldownId ->
                            { state with
                                UserCooldownsStorage =
                                    Model.UserCooldownsStorage.set
                                        userCooldownId
                                        (fun x ->
                                            { x with
                                                LastDateTimeActivated = System.DateTimeOffset.UtcNow
                                            }
                                        )
                                        state.UserCooldownsStorage
                            }

                    match whomId with
                    | Some whomId ->
                        send (Some whomId)
                    | None ->
                        match e.Message.ReferencedMessage with
                        | null ->
                            send whomId
                        | referencedMessage ->
                            send (Some referencedMessage.Author.Id)

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

let create db =
    let m =
        let commands = Model.Commands.init "customCommands" db
        let init = {
            Commands = commands
            CommandsParser = CommandsParser.create commands.Cache
            UserCooldownsStorage =
                Model.UserCooldownsStorage.init "customCommandsUserCooldowns" db
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

    { BotModule.empty with
        MessageCreateEventHandle =
            let handle (client, e) =
                m.Post (NewMessageHandle (client, e))
            Some handle
    }
