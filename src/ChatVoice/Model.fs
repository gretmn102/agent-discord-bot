module ChatVoice.Model
open Types

type VoiceChannelId = ChannelId
type Settings = Map<GuildId, Map<VoiceChannelId, ChannelId>>
