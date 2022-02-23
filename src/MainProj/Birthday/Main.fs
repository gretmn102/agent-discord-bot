module Birthday.Main
open FsharpMyExtension
open FsharpMyExtension.Either
open DSharpPlus

open Types
open Model

type SettingMsg =
    | SetRole of RoleId

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
        >>. (pmentionRole <|> puint64)

    let pbirthdaySet: _ Parser =
        skipStringCI "birthdaySet" >>. spaces
        >>. tuple2 pint32 pint32

    let start: _ Parser =
        let psetting =
            pbirthdaySetRole |>> SetRole
        let pbirthday =
            pbirthdaySet |>> fun (day, month) -> SetBirthday { Day = day; Month = month }
        choice [
            psetting |>> SettingReq
            pbirthday |>> BirthdayReq
        ]

let settingReduce
    (e: EventArgs.MessageCreateEventArgs)
    (msg: SettingMsg)
    (state: BirthdaySetting.GuildBirthdaySetting) =

    match msg with
    | SetRole newRoleId ->
        let currentMember = await (e.Guild.GetMemberAsync(e.Author.Id))
        let replyMessage =
            await (e.Channel.SendMessageAsync("Processing..."))

        if (currentMember.Permissions &&& Permissions.Administrator = Permissions.Administrator) then
            match Map.tryFind e.Guild.Id state with
            | Some settings ->
                let settings =
                    { settings with
                        RoleId = newRoleId
                    }

                BirthdaySetting.replace settings
                let state = Map.add e.Guild.Id settings state

                awaiti (replyMessage.ModifyAsync (Entities.Optional "Done!"))

                state
            | None ->
                let settings = BirthdaySetting.insert (e.Guild.Id, newRoleId)
                let state = Map.add e.Guild.Id settings state

                awaiti (replyMessage.ModifyAsync (Entities.Optional "Done!"))

                state
        else
            awaiti (replyMessage.ModifyAsync(Entities.Optional("You don't have administration permission")))

            state


type JobType = {
    RemoveBirthdayRoles: UserId Set
    Birthdays: UserId Set
}

let addJob (scheduler: Scheduler.Scheduler<JobType>) (dayMonth: Birthday.DayMonth) userId =
    let date = System.DateTime(System.DateTime.Now.Year, dayMonth.Month, dayMonth.Day)

    let value =
        match scheduler.GetJob date with
        | None ->
            {
                Birthdays = Set.singleton userId
                RemoveBirthdayRoles = Set.empty
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
    (state: Birthday.UsersBirthday) =

    let replyMessage =
        await (e.Channel.SendMessageAsync("Processing..."))

    match msg with
    | SetBirthday dayMonth ->
        let authorId = e.Author.Id

        match Map.tryFind authorId state with
        | None ->
            let birthday = Birthday.insert (authorId, dayMonth)
            let state = Map.add authorId birthday state

            addJob scheduler dayMonth authorId

            awaiti (replyMessage.ModifyAsync (Entities.Optional "Done!"))
            state

        | Some oldData ->
            let oldData =
                { oldData with
                    Date = dayMonth
                }

            Birthday.replace oldData
            let state = Map.add authorId oldData state

            removeJob scheduler oldData.Date authorId
            addJob scheduler dayMonth authorId

            awaiti (replyMessage.ModifyAsync (Entities.Optional "Done!"))
            state

type State =
    {
        BirthdaySetting: BirthdaySetting.GuildBirthdaySetting
        Birthdays: Birthday.UsersBirthday
    }

type InternalReq =
    | GetGuilds of AsyncReplyChannel<BirthdaySetting.GuildBirthdaySetting>

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
        BirthdaySetting = BirthdaySetting.getAll ()
        Birthdays = Birthday.getAll ()
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

let exec e msg =
    m.Post(UsersReq (e, msg))

let startAsync (client: DiscordClient) =
    Scheduler.startAsync scheduler 500 (fun job ->
        let tasks = job.Type
        let guilds = m.PostAndReply(fun r -> InternalReq (GetGuilds r))
        guilds
        |> Seq.iter (fun (KeyValue(guildId, v)) ->

            let guild = await (client.GetGuildAsync guildId)
            // TODO: tasks.Birthdays
            tasks.Birthdays
            guild.GetMemberAsync
            ()
        )
        // TODO: tasks.RemoveBirthdayRoles
    )
