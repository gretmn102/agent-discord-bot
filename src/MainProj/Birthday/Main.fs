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

let birthdayReduce
    (e: EventArgs.MessageCreateEventArgs)
    (msg: BirthdayMsg)
    (state: Birthday.UsersBirthday) =

    let replyMessage =
        await (e.Channel.SendMessageAsync("Processing..."))

    match msg with
    | SetBirthday dayMonth ->
        match Map.tryFind e.Author.Id state with
        | None ->
            let birthday = Birthday.insert (e.Author.Id, dayMonth)

            Scheduler.add dayMonth

            let state = Map.add e.Author.Id birthday state

            awaiti (replyMessage.ModifyAsync (Entities.Optional "Done!"))
            state

        | Some oldData ->
            let oldData =
                { oldData with
                    Date = dayMonth
                }

            Birthday.replace oldData
            let state = Map.add e.Author.Id oldData state

            Scheduler.remove oldData.Date
            Scheduler.add dayMonth

            awaiti (replyMessage.ModifyAsync (Entities.Optional "Done!"))
            state

type State =
    {
        BirthdaySetting: BirthdaySetting.GuildBirthdaySetting
        Birthdays: Birthday.UsersBirthday
    }

let reduce (e, msg) state =
    match msg with
    | SettingReq msg ->
        { state with
            BirthdaySetting =
                settingReduce e msg state.BirthdaySetting
        }

    | BirthdayReq msg ->
        { state with
            Birthdays =
                birthdayReduce e msg state.Birthdays
        }

let m =
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
                        reduce msg state
                    with e ->
                        printfn "%A" e
                        state

                return! loop state
            }
        loop init
    )

let exec e msg =
    m.Post(e, msg)
