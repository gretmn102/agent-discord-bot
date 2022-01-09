namespace DiscordMessage
open Types

type CustomEmoji =
    {
        Id: EmojiId
        Animated: bool
        Name: string
    }

module Parser =
    open FParsec

    let puserMention<'u> : Parser<UserId, 'u> =
        skipString "<@" >>. optional (skipChar '!') >>. puint64 .>> skipChar '>'

    let puserMentionTargetStr<'u> (userId: string) : Parser<_, 'u> =
        skipString "<@" >>. optional (skipChar '!') >>. skipString userId >>. skipChar '>'

    let puserMentionTarget<'u> (userId: UserId) : Parser<_, 'u> =
        puserMentionTargetStr (string userId)

    let pmentionRole<'u> : Parser<RoleId, 'u> =
        skipString "<@&" >>. puint64 .>> skipChar '>'

    let pmentionRoleTargetStr<'u> (roleId: string): Parser<_, 'u> =
        skipString "<@&" >>. skipString roleId .>> skipChar '>'

    let pmentionRoleTarget<'u> (roleId: RoleId): Parser<_, 'u> =
        pmentionRoleTargetStr (string roleId)

    let pchannelMention<'u> : Parser<ChannelId, 'u> =
        skipString "<#" >>. puint64 .>> skipChar '>'

    let pchannelMentionTargetRaw<'u> (channelId: string): Parser<_, 'u> =
        skipString "<#" >>. skipString channelId .>> skipChar '>'

    let pchannelMentionTarget<'u> (channelId: ChannelId): Parser<_, 'u> =
        pchannelMentionTargetRaw (string channelId)

    let pcustomEmoji<'u> : Parser<_, 'u> =
        pipe3
            (stringReturn "<:" false <|> stringReturn "<a:" true)
            (manySatisfy ((<>) ':') .>> skipChar ':')
            (puint64 .>> pchar '>')
            (fun animated name id ->
                {
                    Id = id
                    Animated = animated
                    Name = name
                }
            )
