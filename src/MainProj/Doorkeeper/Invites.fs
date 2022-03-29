module Doorkeeper.Invites
open FsharpMyExtension
open FsharpMyExtension.Either
open DSharpPlus

open Types

type InvitesState = Map<GuildId, Map<string, Entities.DiscordInvite>>

type State =
    {
        InvitesState: InvitesState
        Setting: Model.InvitesSetting.GuildSetting
    }

type Request =
    | SetSetting of ChannelId
    | GetSetting

module Parser =
    open FParsec

    open DiscordMessage.Parser

    type 'Result Parser = Primitives.Parser<'Result, unit>

    let setChannelName = "setInviteChannel"
    let getChannelName = "getInviteChannel"

    let start: _ Parser =
        choice [
            skipStringCI setChannelName >>. spaces >>. (puint64 <|> pchannelMention) |>> SetSetting
            skipStringCI getChannelName >>% GetSetting
        ]

let reduceRequest (e: EventArgs.MessageCreateEventArgs) (req: Request) (state: Model.InvitesSetting.GuildSetting) =
    match req with
    | SetSetting channelId ->
        let guild = e.Guild
        let currentMember = await (guild.GetMemberAsync e.Author.Id)
        let replyMessage =
            await (e.Channel.SendMessageAsync "Processing...")

        if currentMember.Permissions &&& Permissions.Administrator = Permissions.Administrator then
            let state =
                match Map.tryFind e.Guild.Id state with
                | Some data ->
                    let data =
                        { data with
                            OutputChannel = channelId
                        }

                    Model.InvitesSetting.replace data

                    Map.add guild.Id data state
                | None ->
                    let x = Model.InvitesSetting.insert (guild.Id, channelId)
                    Map.add guild.Id x state

            awaiti (replyMessage.ModifyAsync(Entities.Optional "Setting has been set"))

            state
        else
            awaiti (replyMessage.ModifyAsync(Entities.Optional "You don't have administrative permission"))
            state
    | GetSetting ->
        let replyMessage =
            await (e.Channel.SendMessageAsync "Processing...")

        match Map.tryFind e.Guild.Id state with
        | Some data ->
            let msg =
                sprintf "<#%d>" data.OutputChannel

            awaiti (replyMessage.ModifyAsync(Entities.Optional msg))
        | None ->
            awaiti (replyMessage.ModifyAsync(Entities.Optional "Settings not set yet"))

        state

type Req =
    | GuildAvailableHandle of Entities.DiscordGuild
    | InviteCreatedHandle of EventArgs.InviteCreateEventArgs
    | InviteDeletedHandle of EventArgs.InviteDeleteEventArgs
    | GuildMemberAddedHandle of EventArgs.GuildMemberAddEventArgs
    | Request of EventArgs.MessageCreateEventArgs * Request

let reduce (req: Req) (state: State) =
    match req with
    | GuildMemberAddedHandle e ->
        match Map.tryFind e.Guild.Id state.Setting with
        | Some setting ->
            match Map.tryFind e.Guild.Id state.InvitesState with
            | Some oldInvites ->
                let newInvites = await (e.Guild.GetInvitesAsync())
                let diff =
                    newInvites
                    |> Seq.filter (fun newInvite ->
                        match Map.tryFind newInvite.Code oldInvites with
                        | Some oldInvite ->
                            newInvite.Uses <> oldInvite.Uses
                        | None ->
                            printfn "Something went wrong"
                            false
                    )
                    |> List.ofSeq

                match diff with
                | [ invite ] ->
                    match e.Guild.GetChannel setting.OutputChannel with
                    | null -> state
                    | channel ->
                        let embed = Entities.DiscordEmbedBuilder()
                        embed.Description <-
                            sprintf "%s пришел на сервер по [ссылке](https://discord.gg/%s), которую создал %s." e.Member.Mention invite.Code invite.Inviter.Mention

                        let b = Entities.DiscordMessageBuilder()
                        b.Embed <- embed.Build ()

                        try
                            awaiti (channel.SendMessageAsync b)
                        with _ -> ()

                        { state with
                            InvitesState =
                                let invites = Map.add invite.Code invite oldInvites
                                Map.add e.Guild.Id invites state.InvitesState }

                | invites ->
                    printfn "Something went wrong:\n%A" invites

                    { state with
                        InvitesState =
                            let invites =
                                invites
                                |> List.fold
                                    (fun oldInvites invite -> Map.add invite.Code invite oldInvites)
                                    oldInvites
                            Map.add e.Guild.Id invites state.InvitesState }

            | None ->
                printfn "Something went wrong"

                state

        | None -> state

    | InviteCreatedHandle e ->
        let inviteCreatedHandle (e: EventArgs.InviteCreateEventArgs) (state: InvitesState) =
            let invites =
                match Map.tryFind e.Guild.Id state with
                | Some invites -> invites
                | None -> Map.empty

            let invites = Map.add e.Invite.Code e.Invite invites
            Map.add e.Guild.Id invites state

        { state with
            InvitesState =
                inviteCreatedHandle e state.InvitesState
        }

    | InviteDeletedHandle e ->
        let inviteDeletedHandle (e: EventArgs.InviteDeleteEventArgs) (state: InvitesState) =
            let invites =
                match Map.tryFind e.Guild.Id state with
                | Some invites -> Map.remove e.Invite.Code invites
                | None -> Map.empty

            Map.add e.Guild.Id invites state

        { state with
            InvitesState =
                inviteDeletedHandle e state.InvitesState
        }

    | GuildAvailableHandle guild ->
        let guildAvailableHandle (guild: Entities.DiscordGuild) (state: InvitesState) =
            let invites =
                try
                    await (guild.GetInvitesAsync())
                with e ->
                    [] :> System.Collections.Generic.IReadOnlyList<_>

            let guildInvites =
                invites
                |> Seq.fold
                    (fun state invite ->
                        Map.add invite.Code invite state
                    )
                    Map.empty

            Map.add guild.Id guildInvites state

        { state with
            InvitesState =
                guildAvailableHandle guild state.InvitesState
        }

    | Request (e, r) ->
        { state with
            Setting =
                reduceRequest e r state.Setting
        }

let m =
    let init = {
        InvitesState = Map.empty
        Setting = Model.InvitesSetting.getAll ()
    }

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

let guildMemberAddedHandle guild =
    m.Post (GuildMemberAddedHandle guild)
let inviteCreatedHandle e =
    m.Post (InviteCreatedHandle e)
let inviteDeletedHandle e =
    m.Post (InviteDeletedHandle e)
let guildAvailableHandle e =
    m.Post (GuildAvailableHandle e)
let exec e r =
    m.Post (Request(e, r))
