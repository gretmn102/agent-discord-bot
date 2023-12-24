module ImageChannel.View
open DSharpPlus
open FsharpMyExtension
open DiscordBotExtensions.Types

let view str =
    let b = Entities.DiscordMessageBuilder()
    b.Content <- str
    b

let channelsView (channelIds: ChannelId []) =
    let b = Entities.DiscordMessageBuilder()
    b.Content <-
        match channelIds with
        | [||] ->
            "Каналы еще не установлены."
        | xs ->
            xs
            |> Array.map (sprintf "* <#%d>")
            |> String.concat "\n"
    b
