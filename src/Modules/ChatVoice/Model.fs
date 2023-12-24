module ChatVoice.Model
open DiscordBotExtensions.Types

type VoiceChannelId = ChannelId
type Settings = Map<GuildId, Map<VoiceChannelId, ChannelId>>
