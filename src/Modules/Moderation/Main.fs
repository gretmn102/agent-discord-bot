module Moderation.Main
open DSharpPlus

open Types

type Request =
    /// `targetUser * reason`
    | Kick of UserId * string option
    /// `targetUser * deleteMessageDays * reason`
    | Ban of UserId * (uint * string option) option

module Parser =
    open FParsec

    open DiscordMessage.Parser

    type 'Result Parser = Primitives.Parser<'Result, unit>

    let template cmdName =
        skipStringCI cmdName >>. spaces
        >>. (puserMention <|> puint64) .>> spaces
        .>>. opt (many1Satisfy (fun _ -> true))

    let pkick: _ Parser = template "kick"

    let pban: _ Parser =
        skipStringCI "ban" >>. spaces
        >>. ((puserMention <|> puint64) .>> spaces)
        .>>. opt (puint32 .>>. (opt (many1Satisfy (fun _ -> true))))

    let start f: _ Parser =
        choice [
            pkick |>> Kick
            pban |>> Ban
        ]
        >>= fun msg ->
            preturn (fun x -> f x msg)

let reduce (e: EventArgs.MessageCreateEventArgs) (msg: Request) =
    match msg with
    | Kick(targetUserId, reason) ->
        awaiti <| e.Channel.TriggerTypingAsync()
        let guild = e.Guild

        let author = getGuildMember guild e.Author

        if (author.Permissions &&& Permissions.Administrator = Permissions.Administrator)
           || (author.Permissions &&& Permissions.KickMembers = Permissions.KickMembers) then

            let targetUser =
                try
                    await <| guild.GetMemberAsync targetUserId
                    |> Ok
                with e -> Error e.Message

            match targetUser with
            | Error _ ->
                sprintf "User <@%d> is not a member of this guild" targetUserId
                |> e.Channel.SendMessageAsync
                |> awaiti

            | Ok targetUser ->
                try
                    match reason with
                    | None ->
                        targetUser.RemoveAsync().GetAwaiter().GetResult()

                        sprintf "<@%d> кикнул <@%d>(%s) без причины." author.Id targetUserId targetUser.Username
                        |> e.Channel.SendMessageAsync
                        |> awaiti
                    | Some reason ->
                        targetUser.RemoveAsync(reason).GetAwaiter().GetResult()

                        sprintf "<@%d> кикнул <@%d>(%s) по причине: %s." author.Id targetUserId targetUser.Username reason
                        |> e.Channel.SendMessageAsync
                        |> awaiti
                with ex ->
                    [
                        sprintf "<@%d>, something wrong" author.Id
                        "```"
                        ex.Message
                        "```"
                    ] |> String.concat "\n"
                    |> e.Channel.SendMessageAsync
                    |> awaiti

        else
            sprintf "<@%d>, you don't have administrative or kick member permission" author.Id
            |> e.Channel.SendMessageAsync
            |> awaiti

    | Ban(targetUserId, args) ->
        awaiti <| e.Channel.TriggerTypingAsync()
        let guild = e.Guild

        let author = getGuildMember guild e.Author

        if (author.Permissions &&& Permissions.Administrator = Permissions.Administrator)
           || (author.Permissions &&& Permissions.BanMembers = Permissions.BanMembers) then

            let targetUser =
                try
                    await <| guild.GetMemberAsync targetUserId
                    |> Ok
                with e -> Error e.Message

            match targetUser with
            | Error _ ->
                sprintf "User <@%d> is not a member of this guild" targetUserId
                |> e.Channel.SendMessageAsync
                |> awaiti

            | Ok targetUser ->
                try
                    match args with
                    | None ->
                        targetUser.BanAsync().GetAwaiter().GetResult()

                        sprintf "<@%d> забанил <@%d>(%s) без причины." author.Id targetUserId targetUser.Username
                        |> e.Channel.SendMessageAsync
                        |> awaiti
                    | Some (deleteMessageDays, reason) ->
                        match reason with
                        | None ->
                            if deleteMessageDays = 0u then
                                targetUser.BanAsync().GetAwaiter().GetResult()

                                sprintf "<@%d> забанил <@%d>(%s) без причины." author.Id targetUserId targetUser.Username
                                |> e.Channel.SendMessageAsync
                                |> awaiti
                            else
                                targetUser.BanAsync(int32 deleteMessageDays).GetAwaiter().GetResult()

                                sprintf "<@%d> забанил <@%d>(%s) с удалением сообщений за %d дней без причины." author.Id targetUserId targetUser.Username deleteMessageDays
                                |> e.Channel.SendMessageAsync
                                |> awaiti

                        | Some reason ->
                            if deleteMessageDays = 0u then
                                targetUser.BanAsync(reason = reason).GetAwaiter().GetResult()

                                sprintf "<@%d> забанил <@%d>(%s) по причине: %s." author.Id targetUserId targetUser.Username reason
                                |> e.Channel.SendMessageAsync
                                |> awaiti
                            else
                                targetUser.BanAsync(int32 deleteMessageDays, reason).GetAwaiter().GetResult()

                                sprintf "<@%d> забанил <@%d>(%s) с удалением сообщений за %d дней по причине: %s." author.Id targetUserId targetUser.Username deleteMessageDays reason
                                |> e.Channel.SendMessageAsync
                                |> awaiti

                with ex ->
                    [
                        sprintf "<@%d>, something wrong" author.Id
                        "```"
                        ex.Message
                        "```"
                    ] |> String.concat "\n"
                    |> e.Channel.SendMessageAsync
                    |> awaiti

        else
            sprintf "<@%d>, you don't have administrative or ban member permission" author.Id
            |> e.Channel.SendMessageAsync
            |> awaiti

let create () =
    { Shared.BotModule.empty with
        MessageCreateEventHandleExclude =
            let exec: MessageCreateEventHandler Parser.Parser =
                Parser.start (fun (client: DiscordClient, e: EventArgs.MessageCreateEventArgs) msg ->
                   reduce e msg
                )
            Some exec
    }
