namespace DiscordMessage
open Types

type CustomEmoji =
    {
        Id: EmojiId
        Animated: bool
        Name: string
    }

type UnicodeOrCustomEmoji =
    | UnicodeEmoji of string
    | CustomEmoji of CustomEmoji

module Parser =
    open FParsec

    let puserMention<'u> : Parser<UserId, 'u> =
        skipString "<@" >>. optional (skipChar '!') >>. puint64 .>> skipChar '>'

    let puserMentionTargetP (p: Parser<_, 'u>) : Parser<_, 'u> =
        skipString "<@" >>? optional (skipChar '!') >>? p .>> skipChar '>'

    let puserMentionTargetStr<'u> (userId: string) : Parser<_, 'u> =
        puserMentionTargetP (skipString userId)

    let puserMentionTarget<'u> (userId: UserId) : Parser<_, 'u> =
        puserMentionTargetStr (string userId)

    let pmentionRole<'u> : Parser<RoleId, 'u> =
        skipString "<@&" >>. puint64 .>> skipChar '>'

    let pmentionRoleTargetStr<'u> (roleId: string): Parser<_, 'u> =
        skipString "<@&" >>? skipString roleId .>> skipChar '>'

    let pmentionRoleTarget<'u> (roleId: RoleId): Parser<_, 'u> =
        pmentionRoleTargetStr (string roleId)

    let pchannelMention<'u> : Parser<ChannelId, 'u> =
        skipString "<#" >>. puint64 .>> skipChar '>'

    let pchannelMentionTargetStr<'u> (channelId: string): Parser<_, 'u> =
        skipString "<#" >>? skipString channelId .>> skipChar '>'

    let pchannelMentionTarget<'u> (channelId: ChannelId): Parser<_, 'u> =
        pchannelMentionTargetStr (string channelId)

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

    let pmessagePath<'u> : Parser<_, 'u> =
        pipe3
            (skipString "https://discord.com/channels/" >>. puint64 .>> pchar '/')
            (puint64 .>> pchar '/')
            puint64
            (fun guildId channelId messageId ->
                {
                    GuildId = guildId
                    ChannelId = channelId
                    MessageId = messageId
                }
            )

    let pemoji<'u> : Parser<_, 'u> =
        pcustomEmoji |>> CustomEmoji <|> (many1Satisfy ((<>) ' ') |>> UnicodeEmoji)

    let pcodeBlock<'u> : Parser<_, 'u> =
        between (skipString "```" .>> skipManySatisfy ((<>) '\n') .>> skipChar '\n')
            (skipString "```")
            (manyStrings (
                many1Satisfy ((<>) '`')
                <|> (notFollowedByString "```" >>. charReturn '`' "`"))
            )

    let pquote<'u> : Parser<_, 'u> =
        between
            (skipChar '"')
            (skipChar '"')
            (many1Strings (
                many1Satisfy (isNoneOf "\"\\")
                <|> (skipChar '\\' >>. ((pchar '"' |>> string) <|>% "\\"))
            ))

module Ext =
    let clearComponents (msg: DSharpPlus.Entities.DiscordMessage) =
        // does not clean components:
        // let content = DSharpPlus.Entities.Optional.FromValue ""
        // awaiti (msg.ModifyAsync content)

        let b = DSharpPlus.Entities.DiscordMessageBuilder()
        // necessary because throw `System.ArgumentException: You must specify content, an embed, a sticker, or at least one file.`
        b.AddEmbeds msg.Embeds |> ignore
        b.Content <- msg.Content
        awaiti (msg.ModifyAsync b)
