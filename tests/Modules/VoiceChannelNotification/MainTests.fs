module Modules.VoiceChannelNotification.Main.Tests

open VoiceChannelNotification.Main.Parser

[<Tests>]
let templateMessageTests =
    testList "templateMessageTests" [
        testCase "templateMessageTests1" (fun _ ->
            let input = "<:lpPepeLol:923837721481469992> <@3847> <@!1234> <@nickName> заш<@userName>ел в <#voiceChannel>!"
            let exp = Right [
                Text "<"; Text ":lpPepeLol:923837721481469992> "
                Text "<"; Text "@3847> "
                Text "<"; Text "@!1234> "
                NickName
                Text " заш"; UserName; Text "ел в "
                VoiceChannel; Text "!"
            ]
            Assert.Equal("", exp, FParsecExt.runEither ptemplateMessage input)
        )
    ]
