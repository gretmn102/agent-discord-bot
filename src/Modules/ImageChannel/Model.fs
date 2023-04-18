module ImageChannel.Model
open FsharpMyExtension
open Types

type Req<'Arg, 'Res, 'Next> = 'Arg * ('Res -> 'Next)

type SettingsReq<'Next> =
    | AddChannel of Req<ChannelId, unit, 'Next>
    | RemoveChannel of Req<ChannelId, bool, 'Next>
    | GetChannels of Req<unit, ChannelId Set, 'Next>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module SettingsReq =
    let addChannel arg next =
        AddChannel(arg, next)

    let removeChannel arg next =
        RemoveChannel(arg, next)

    let getChannels arg next =
        GetChannels(arg, next)

type Cmd =
    | MarriedCouplesCm of SettingsReq<Cmd>
    | Print of Req<{| IsEphemeral: bool; Description: string |}, unit, Cmd>
    | UserIsBot of Req<UserId, bool, Cmd>
    | CreateChannelsView of Req<ChannelId [], unit, Cmd>
    | End
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Cmd =
    let apply fn arg next =
        MarriedCouplesCm (fn arg (fun res ->
            next res
        ))

    let print isEphemeral description next =
        let args = {| IsEphemeral = isEphemeral; Description = description |}
        Print(args, fun () ->
            next ()
        )

    let userIsBot userId next =
        UserIsBot(userId, next)

    let createChannelsView arg next =
        CreateChannelsView(arg, next)

let addChannel (channelId: ChannelId) =
    pipeBackwardBuilder {
        do! Cmd.apply SettingsReq.addChannel channelId
        do! Cmd.print true
                (sprintf "Теперь бот будет удалять сообщения без картинок в <#%d>." channelId)
        return End
    }

let removeChannel (channelId: ChannelId) =
    pipeBackwardBuilder {
        let! isRemoved = Cmd.apply SettingsReq.removeChannel channelId
        if isRemoved then
            do! Cmd.print true
                    (sprintf "Теперь бот будет **не** будет удалять сообщения без картинок в <#%d>." channelId)
            return End
        else
            do! Cmd.print true
                    (sprintf "Канал <#%d> и так не рассматривался ботом." channelId)
            return End
    }

let getChannels =
    pipeBackwardBuilder {
        let! channels = Cmd.apply SettingsReq.getChannels ()
        let channels = Set.toArray channels
        do! Cmd.createChannelsView channels
        return End
    }

[<RequireQualifiedAccess>]
type MessageCreateHandleCmd =
    | SettingsReq of SettingsReq<MessageCreateHandleCmd>
    | AuthorIsBot of Req<unit, bool, MessageCreateHandleCmd>
    | RemoveMessage of Req<unit, Result<unit, string>, MessageCreateHandleCmd>
    | MessageHasAttachment of Req<unit, bool, MessageCreateHandleCmd>
    | End
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module MessageCreateHandleCmd =
    let apply fn arg next =
        MessageCreateHandleCmd.SettingsReq (fn arg (fun res ->
            next res
        ))

    let removeMessage userId next =
        MessageCreateHandleCmd.RemoveMessage(userId, next)

    let authorIsBot userId next =
        MessageCreateHandleCmd.AuthorIsBot(userId, next)

    let messageHasAttachment userId next =
        MessageCreateHandleCmd.MessageHasAttachment(userId, next)

let messageCreateHandle (channel: ChannelId) (authorId: UserId) messageId =
    let testAuthorIsNotBot next =
        pipeBackwardBuilder {
            let! isBot = MessageCreateHandleCmd.authorIsBot ()
            if isBot then
                return MessageCreateHandleCmd.End
            else
                return next ()
        }

    let testChannelIsEnabled next =
        pipeBackwardBuilder {
            let! channels = MessageCreateHandleCmd.apply SettingsReq.getChannels ()
            if Set.contains channel channels then
                return next ()
            else
                return MessageCreateHandleCmd.End
        }

    let testMessageHasAttachment next =
        pipeBackwardBuilder {
            let! has = MessageCreateHandleCmd.messageHasAttachment ()
            if has then
                return MessageCreateHandleCmd.End
            else
                return next ()
        }

    pipeBackwardBuilder {
        do! testAuthorIsNotBot
        do! testChannelIsEnabled
        do! testMessageHasAttachment
        let! result = MessageCreateHandleCmd.removeMessage ()
        do
            match result with
            | Error errMsg ->
                printfn "%A" errMsg // TODO
            | _ -> ()

        return MessageCreateHandleCmd.End
    }

module GuildSettingsDb =
    open MongoDB.Driver
    open MongoDB.Bson
    open Db

    type MainData =
        {
            Channels: ChannelId Set
        }
        static member Init channels =
            {
                Channels = channels
            }
        static member Empty =
            {
                Channels = Set.empty
            }

    type Version =
        | V0 = 0

    type Id = GuildId

    type Data = CommonDb.Data<Id, Version, MainData>
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Data =
        let create id data : Data =
            CommonDb.Data.create id Version.V0 data

    type GuildData = CommonDb.GuildData<Id, Version, MainData>
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module GuildData =
        let createData id =
            Data.create id MainData.Empty

        let init collectionName (db: IMongoDatabase): GuildData =
            CommonDb.GuildData.init
                createData
                (fun ver x ->
                    match Option.get ver with
                    | Version.V0 ->
                        None, Serialization.BsonSerializer.Deserialize<Data>(x)
                    | x ->
                        failwithf "Version = %A not implemented" x
                )
                collectionName
                db

        let set id setAdditionParams (guildData: GuildData) =
            CommonDb.GuildData.set
                createData
                id
                setAdditionParams
                guildData

        let sets (items: Data seq) db =
            CommonDb.GuildData.sets
                items
                db

        let drop (db: IMongoDatabase) (items: GuildData) =
            CommonDb.GuildData.drop db items

        let tryFindById id (items: GuildData): Data option =
            CommonDb.GuildData.tryFind id items

        let removeByIds ids (items: GuildData) =
            CommonDb.GuildData.removeByIds ids items

    let interp guildId req (guildSettings: GuildData) =
        let createId id = id

        match req with
        | AddChannel(channelId, next) ->
            let id = createId guildId
            let guildSettings =
                guildSettings
                |> GuildData.set id (fun x ->
                    { x with
                        Channels = Set.add channelId x.Channels
                    }
                )
            let req = next ()
            req, guildSettings
        | RemoveChannel(channelId, next) ->
            let id = createId guildId

            let findGuildSettings () next' =
                match GuildData.tryFindById id guildSettings with
                | Some data ->
                    next' data
                | None ->
                    next false, guildSettings

            let testChannelIsContains (data: Data) next' =
                if Set.contains channelId data.Data.Channels then
                    next' ()
                else
                    next false, guildSettings

            pipeBackwardBuilder {
                let! data = findGuildSettings ()
                do! testChannelIsContains data

                let guildSettings =
                    guildSettings
                    |> GuildData.set id (fun x ->
                        { x with
                            Channels = Set.remove channelId x.Channels
                        }
                    )

                return next true, guildSettings
            }

        | GetChannels((), f) ->
            let id = createId guildId
            let res =
                match GuildData.tryFindById id guildSettings with
                | Some data ->
                    data.Data.Channels
                | None ->
                    Set.empty

            let req = f res
            req, guildSettings
