module VoiceChannelNotification.Main
open FsharpMyExtension
open DSharpPlus

open Types
open Model

let settings: Settings =
    [
        914459693924110356UL, {
            OutputChannelId = 914459694758772777UL
            UserMessages = [
                let voiceId = 914459694758772781UL
                voiceId, [
                    let message =
                        let b = Entities.DiscordMessageBuilder()

                        let embed = Entities.DiscordEmbedBuilder()
                        embed.Color <- Entities.Optional.FromValue(Entities.DiscordColor("#ffeb3b"))
                        embed.Description <-
                            [
                                sprintf "<:lpPepeLol:923837721481469992> Светлана зашла в <#%d>!" voiceId
                            ] |> String.concat "\n"

                        b.Embed <- embed.Build()
                        b

                    807631911131807765UL, message
                ] |> Map.ofList
            ] |> Map.ofList
        }

        878956153537691658UL, {
            OutputChannelId = 880470210564468747UL
            UserMessages = [
                let voiceId = 878956154049429596UL
                voiceId, [
                    let message =
                        let b = Entities.DiscordMessageBuilder()

                        let embed = Entities.DiscordEmbedBuilder()
                        embed.Color <- Entities.Optional.FromValue(Entities.DiscordColor("#ffeb3b"))
                        embed.Description <-
                            [
                                sprintf "<:lpPepeLol:923837721481469992> Агент зашел в <#%d>!" voiceId
                            ] |> String.concat "\n"

                        b.Embed <- embed.Build()
                        b

                    796931597898088448UL, message
                ] |> Map.ofList
            ] |> Map.ofList
        }
    ] |> Map.ofList

let voiceHandle (e: EventArgs.VoiceStateUpdateEventArgs) =
    match Map.tryFind e.Guild.Id settings with
    | Some setting ->
        match e.After with
        | null -> ()
        | after ->
            match after.Channel with
            | null -> ()
            | channel ->
                match Map.tryFind channel.Id setting.UserMessages with
                | Some userMessages ->
                    match Map.tryFind e.User.Id userMessages with
                    | Some customUserMessage ->
                        if not (Seq.length channel.Users > 1) then
                            let outputChannel = e.Guild.GetChannel(setting.OutputChannelId)

                            awaiti (outputChannel.SendMessageAsync(customUserMessage))
                    | None -> ()
                | None -> ()
    | None -> ()
