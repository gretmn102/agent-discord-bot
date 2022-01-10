module VoiceChannelNotification.Model
open FsharpMyExtension
open MongoDB.Driver
open MongoDB.Bson

open Types
open Db

module VoiceNotification =
    type VoiceChannelId = ChannelId

    type VoiceNotificationData =
        {
            mutable Id: ObjectId
            mutable GuildId: GuildId
            mutable OutputChannelId: ChannelId option
            mutable TemplateMessage: string option
        }
        static member Init
            (
                guildId: GuildId,
                outputChannelId: ChannelId option,
                templateMessage: string option
            ) =

            {
                Id = ObjectId.Empty
                GuildId = guildId
                OutputChannelId = outputChannelId
                TemplateMessage = templateMessage

            }

    let roles = database.GetCollection<VoiceNotificationData>("voiceNotifications")

    type GuildVoiceNotification = Map<GuildId, VoiceNotificationData>

    let getAll (): GuildVoiceNotification =
        roles.Find(fun x -> true).ToEnumerable()
        |> Seq.fold
            (fun st x ->
                Map.add x.GuildId x st
            )
            Map.empty

    let replace (newRoleData: VoiceNotificationData) =
        roles.ReplaceOne((fun x -> x.Id = newRoleData.Id), newRoleData)
        |> ignore

    let insert
        (
            guildId: GuildId,
            outputChannelId: ChannelId option,
            templateMessage: string option
        ) =

        let x = VoiceNotificationData.Init(guildId, outputChannelId, templateMessage)
        roles.InsertOne(x)
        x

    let remove (roleData: VoiceNotificationData) =
        roles.DeleteOne(fun x -> x.Id = roleData.Id)
        |> ignore
