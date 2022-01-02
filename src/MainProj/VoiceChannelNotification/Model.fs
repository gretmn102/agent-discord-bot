module VoiceChannelNotification.Model
open Types

type VoiceChannelId = ChannelId

type T =
    {
        OutputChannelId: ChannelId
        UserMessages: Map<VoiceChannelId, Map<UserId, DSharpPlus.Entities.DiscordMessageBuilder>>
    }

type Settings = Map<GuildId, T>
