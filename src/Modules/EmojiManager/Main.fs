module EmojiManager.Main
open FsharpMyExtension
open FsharpMyExtension.Either
open DSharpPlus

open Types
open Extensions

type RawOrUnicodeOrCustomEmoji = Choice<EmojiId, DiscordMessage.UnicodeOrCustomEmoji>
module RawOrUnicodeOrCustomEmoji =
    module Parser =
        open FParsec

        open DiscordMessage.Parser

        type 'Result Parser = Primitives.Parser<'Result, unit>

        let start: _ Parser =
            ((puint64 |>> Choice1Of2) <|> (pemoji |>> Choice2Of2))

type EmojiRoleCmd =
    | Set of RawOrUnicodeOrCustomEmoji * RoleId list
    | Get of RawOrUnicodeOrCustomEmoji
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module EmojiRoleCmd =
    module Parser =
        open FParsec

        open DiscordMessage.Parser

        type 'Result Parser = Primitives.Parser<'Result, unit>

        let psetEmojiRole: _ Parser =
            pstringCI "setEmojiRoles" >>. spaces
            >>. tuple2
                    (RawOrUnicodeOrCustomEmoji.Parser.start .>> spaces)
                    (many (puint64 <|> pmentionRole .>> spaces))

        let pgetEmojiRole: _ Parser =
            pstringCI "getEmojiRoles" >>. spaces
            >>. RawOrUnicodeOrCustomEmoji.Parser.start

        let start f: _ Parser =
            choice [
                psetEmojiRole |>> Set
                pgetEmojiRole |>> Get
            ]
            >>= fun msg ->
                preturn (fun x -> f x msg)

type State = unit
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module State =
    let init: State = ()

type Msg =
    | EmojiRoleCmd of EventArgs.MessageCreateEventArgs * EmojiRoleCmd

let reduce (msg: Msg) (state: State): State =
    match msg with
    | EmojiRoleCmd (e, cmd) ->
        let send msg =
            let embed = Entities.DiscordEmbedBuilder()
            embed.Color <- Entities.Optional.FromValue(DiscordEmbed.backgroundColorDarkTheme)
            embed.Description <- msg

            let b = Entities.DiscordMessageBuilder()
            b.Embed <- embed.Build()

            awaiti <| e.Channel.SendMessageAsync b

        awaiti <| e.Channel.TriggerTypingAsync()

        let guild = e.Guild

        let getEmoji (emojiId: EmojiId) =
            try
                await <| guild.GetEmojiAsync emojiId
                |> Ok
            with e ->
                // TODO: handle disconnect
                Error "Такого эмодзи не существует."

        let getCustomEmoji (unicodeOrCustomEmoji: RawOrUnicodeOrCustomEmoji) next =
            match unicodeOrCustomEmoji with
            | Choice1Of2 emojiId -> next emojiId
            | Choice2Of2 unicodeOrCustomEmoji ->
                match unicodeOrCustomEmoji with
                | DiscordMessage.CustomEmoji customEmoji ->
                    next customEmoji.Id
                | DiscordMessage.UnicodeEmoji standardEmoji ->
                    sprintf "Стандартный эмодзи \"%s\" нельзя ограничить ролями, нужен серверный эмодзи." standardEmoji
                    |> send

        let getEmoji (emojiId: EmojiId) next =
            match getEmoji emojiId with
            | Ok emoji -> next emoji
            | Error errMsg ->
                send errMsg

        let getRoles (roleIds: RoleId list) =
            let rec loop acc = function
                | roleId::roleIds ->
                    match guild.GetRole roleId with
                    | null ->
                        Error (sprintf "Не найдена роль %d ID" roleId)
                    | role ->
                        loop (role::acc) roleIds
                | [] ->
                    Ok (List.rev acc)
            loop [] roleIds

        let getRoles (roleIds: RoleId list) next =
            match getRoles roleIds with
            | Ok roles -> next roles
            | Error errMsg -> send errMsg

        let checkPermission (guildMember: Entities.DiscordMember) next =
            if guildMember.Permissions &&& Permissions.Administrator = Permissions.Administrator then
                next ()
            else
                "Для использования этой команды нужны административные права."
                |> send

        let modifyEmoji (emoji: Entities.DiscordGuildEmoji) (roles: Entities.DiscordRole seq) =
            try
                awaiti <| emoji.ModifyAsync(emoji.Name, roles)
                Ok ()
            with e ->
                // TODO: handle disconnect
                "У меня нет прав, чтобы изменять роли."
                |> Error


        let modifyEmoji (emoji: Entities.DiscordGuildEmoji) (roles: Entities.DiscordRole seq) next =
            match modifyEmoji emoji roles with
            | Ok () -> next ()
            | Error errMsg ->
                send errMsg

        match cmd with
        | Set(rawUnicodeOrCustomEmoji, roleIds) ->
            checkPermission (getGuildMember guild e.Author) <| fun () ->
            getCustomEmoji rawUnicodeOrCustomEmoji <| fun emojiId ->
            getEmoji emojiId <| fun emoji ->

            if List.isEmpty roleIds then
                modifyEmoji emoji Seq.empty <| fun () ->
                sprintf "Эмодзи теперь доступен <:%s:%d> всем." emoji.Name emoji.Id
                |> send
            else
                getRoles roleIds <| fun roles ->
                modifyEmoji emoji roles <| fun () ->

                [
                    sprintf "Эмодзи теперь доступен <:%s:%d> только пользователям со следующими ролями:" emoji.Name emoji.Id
                    yield! roleIds |> Seq.map (sprintf "<@&%d>")
                ]
                |> String.concat "\n"
                |> send

        | Get rawUnicodeOrCustomEmoji ->
            getCustomEmoji rawUnicodeOrCustomEmoji <| fun emojiId ->
            getEmoji emojiId <| fun emoji ->
            let emojiRoleIds = emoji.Roles
            if emojiRoleIds.Count > 0 then
                [
                    sprintf "Эмодзи <:%s:%d> доступен для следующих ролей:" emoji.Name emoji.Id
                    yield! emojiRoleIds |> Seq.map (sprintf "<@&%d>")
                ]
                |> String.concat "\n"
                |> send
            else
                sprintf "Эмодзи <:%s:%d> доступен для всех." emoji.Name emoji.Id
                |> send

        state

let create () =
    let m =
        let init = State.init

        MailboxProcessor.Start (fun mail ->
            let rec loop (state: State) =
                async {
                    let! msg = mail.Receive()
                    let state =
                        try
                            reduce msg state
                        with e ->
                            printfn "%A" e
                            state

                    return! loop state
                }
            loop init
        )

    { Shared.BotModule.empty with
        MessageCreateEventHandleExclude =
            let exec: _ EmojiRoleCmd.Parser.Parser =
                EmojiRoleCmd.Parser.start (fun (client: DiscordClient, e: EventArgs.MessageCreateEventArgs) msg ->
                    m.Post (EmojiRoleCmd (e, msg))
                )
            Some exec
    }
