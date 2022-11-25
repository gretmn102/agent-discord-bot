module Birthday.Main
open FsharpMyExtension
open FsharpMyExtension.Either
open DSharpPlus

open Types
open Model

type SettingMsg =
    | SetRole of RoleId option

type BirthdayMsg =
    | SetBirthday of Birthday.DayMonth

type Request =
    | SettingReq of SettingMsg
    | BirthdayReq of BirthdayMsg

module Parser =
    open FParsec

    open DiscordMessage.Parser

    type 'Result Parser = Primitives.Parser<'Result, unit>

    let pbirthdaySetRole: _ Parser =
        skipStringCI "birthdaySetRole" >>. spaces
        >>. opt (pmentionRole <|> puint64)

    let pbirthdaySet: _ Parser =
        skipStringCI "birthdaySet" >>. spaces
        >>. tuple2 pint32 pint32

    let start f: _ Parser =
        let psetting =
            pbirthdaySetRole |>> SetRole
        let pbirthday =
            pbirthdaySet |>> fun (day, month) -> SetBirthday { Day = day; Month = month }
        choice [
            psetting |>> SettingReq
            pbirthday |>> BirthdayReq
        ]
        >>= fun msg ->
            preturn (fun x -> f x msg)

let settingReduce
    (e: EventArgs.MessageCreateEventArgs)
    (msg: SettingMsg)
    (state: BirthdaySetting.Commands) =

    match msg with
    | SetRole newRoleId ->
        let currentMember = getGuildMember e.Guild e.Author
        let replyMessage =
            await (e.Channel.SendMessageAsync("Processing..."))

        if (currentMember.Permissions &&& Permissions.Administrator = Permissions.Administrator) then
            let state =
                state
                |> BirthdaySetting.Commands.set
                    e.Guild.Id
                    (fun data ->
                        { data with
                            RoleId = newRoleId
                        }
                    )

            awaiti (replyMessage.ModifyAsync (Entities.Optional "Done!"))

            state
        else
            awaiti (replyMessage.ModifyAsync(Entities.Optional("You don't have administration permission")))

            state


type JobType = {
    RemoveBirthdayRoles: Map<GuildId, UserId Set>
    Birthdays: UserId Set
}

let addJob (scheduler: Scheduler.Scheduler<JobType>) (dayMonth: Birthday.DayMonth) userId =
    let date = System.DateTime(System.DateTime.Now.Year, dayMonth.Month, dayMonth.Day)

    let value =
        match scheduler.GetJob date with
        | None ->
            {
                Birthdays = Set.singleton userId
                RemoveBirthdayRoles = Map.empty
            }
        | Some x ->
            { x.Type with
                Birthdays = Set.add userId x.Type.Birthdays
            }

    scheduler.AddJob {
        Time = date
        Type = value
    }

let removeJob (scheduler: Scheduler.Scheduler<JobType>) (dayMonth: Birthday.DayMonth) userId =
    let oldDate = System.DateTime(System.DateTime.Now.Year, dayMonth.Month, dayMonth.Day)

    match scheduler.GetJob oldDate with
    | Some x ->
        let value =
            { x.Type with
                Birthdays = Set.remove userId x.Type.Birthdays
            }
        scheduler.AddJob {
            Time = oldDate
            Type = value
        }
    | None ->
        // TODO
        ()

let birthdayReduce
    (scheduler: Scheduler.Scheduler<JobType>)
    (e: EventArgs.MessageCreateEventArgs)
    (msg: BirthdayMsg)
    (state: Birthday.Commands) =

    let replyMessage =
        await (e.Channel.SendMessageAsync("Processing..."))

    match msg with
    | SetBirthday dayMonth ->
        let authorId = e.Author.Id

        match Birthday.Commands.tryFindById authorId state with
        | None ->
            let state =
                state
                |> Birthday.Commands.set
                    authorId
                    (fun data ->
                        { data with
                            Date = dayMonth
                        }
                    )

            addJob scheduler dayMonth authorId

            awaiti (replyMessage.ModifyAsync (Entities.Optional "Done!"))
            state

        | Some oldData ->
            let state =
                state
                |> Birthday.Commands.set
                    authorId
                    (fun data ->
                        { data with
                            Date = dayMonth
                        }
                    )

            removeJob scheduler oldData.Data.Date authorId
            addJob scheduler dayMonth authorId

            awaiti (replyMessage.ModifyAsync (Entities.Optional "Done!"))
            state

type State =
    {
        BirthdaySetting: BirthdaySetting.Commands
        Birthdays: Birthday.Commands
    }

type InternalReq =
    | GetGuilds of AsyncReplyChannel<BirthdaySetting.Commands>

type Req =
    | UsersReq of EventArgs.MessageCreateEventArgs * Request
    | InternalReq of InternalReq

let reduce (scheduler: Scheduler.Scheduler<JobType>) req state =
    match req with
    | UsersReq (e, msg) ->
        match msg with
        | SettingReq msg ->
            { state with
                BirthdaySetting =
                    settingReduce e msg state.BirthdaySetting
            }

        | BirthdayReq msg ->
            { state with
                Birthdays =
                    birthdayReduce scheduler e msg state.Birthdays
            }
    | InternalReq req ->
        match req with
        | GetGuilds r ->
            r.Reply state.BirthdaySetting

            state


let scheduler = new Scheduler.Scheduler<JobType>(Scheduler.State.Empty)

let m: MailboxProcessor<Req> =
    let init = {
        BirthdaySetting = BirthdaySetting.Commands.init "birthdaysSetting" Db.database
        Birthdays = Birthday.Commands.init "birthdaysSetting" Db.database
    }

    MailboxProcessor.Start (fun mail ->
        let rec loop (state: State) =
            async {
                let! msg = mail.Receive()
                let state =
                    try
                        reduce scheduler msg state
                    with e ->
                        printfn "%A" e
                        state

                return! loop state
            }
        loop init
    )

let exec: MessageCreateEventHandler Parser.Parser =
    Parser.start (fun (client: DiscordClient, e: EventArgs.MessageCreateEventArgs) msg ->
        m.Post(UsersReq (e, msg))
    )

let startAsync (client: DiscordClient) =
    Scheduler.startAsync scheduler 500 (fun job ->
        let tasks = job.Type
        let guilds = m.PostAndReply(fun r -> InternalReq (GetGuilds r))

        let guildUserIds =
            guilds.Cache
            |> Seq.choose (fun (KeyValue(guildId, v)) ->
                match await (client.GetGuildAsync guildId) with
                | null -> None // TODO: make sure it works
                | guild ->
                    match v.Data.RoleId with
                    | Some roleId ->
                        match guild.GetRole roleId with
                        | null -> None
                        | role ->
                            let userIds =
                                tasks.Birthdays
                                |> Seq.choose (fun userId ->
                                    match await (guild.GetMemberAsync userId) with
                                    | null -> None
                                    | currentMember ->
                                        try
                                            awaiti <| currentMember.GrantRoleAsync role
                                            Some userId
                                        with e ->
                                            None
                                )
                                |> List.ofSeq

                            if List.isEmpty userIds then None
                            else Some(guildId, userIds)
                    | None ->
                        None
            )

        let nextDay = job.Time.AddDays 1.
        match scheduler.GetJob nextDay with
        | None ->
            {
                Scheduler.Time = nextDay
                Scheduler.Type =
                    {
                        RemoveBirthdayRoles =
                            guildUserIds
                            |> Seq.map (fun (guildId, userIds) ->
                                guildId, Set.ofList userIds
                            )
                            |> Map.ofSeq
                        Birthdays = Set.empty
                    }
            }
        | Some job ->
            { job with
                Type =
                    { job.Type with
                        RemoveBirthdayRoles =
                            guildUserIds
                            |> Seq.fold
                                (fun st (guildId, userIds) ->
                                    st
                                    |> Map.addOrModWith
                                        guildId
                                        (fun () -> Set.ofList userIds)
                                        (fun st ->
                                            userIds |> List.fold (flip Set.add) st
                                        )
                                )
                                job.Type.RemoveBirthdayRoles
                    }
            }
        |> scheduler.AddJob

        // transfered to the next year
        let nextYearDate = job.Time.AddYears 1
        let value =
            match scheduler.GetJob nextYearDate with
            | None ->
                {
                    RemoveBirthdayRoles = Map.empty
                    Birthdays = job.Type.Birthdays
                }
            | Some x ->
                { x.Type with
                    Birthdays =
                        job.Type.Birthdays |> Set.fold (flip Set.add) x.Type.Birthdays
                }

        scheduler.AddJob {
            Time = nextYearDate
            Type = value
        }

        // TODO: tasks.RemoveBirthdayRoles
    )
