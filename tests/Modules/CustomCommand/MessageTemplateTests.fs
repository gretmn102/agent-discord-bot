module Modules.CustomCommand.MessageTemplate.Tests
open Fuchu
open FsharpMyExtension

open CustomCommand.MessageTemplate

[<Tests>]
let ``CustomCommand.MessageTemplate.Message.deserialize`` =
    testList "CustomCommand.MessageTemplate.Message.deserialize" [
        testCase "base" <| fun () ->
            let act =
                Message.parse
                    "<:someEmoji:12345> text <@authorName> <@authorMention> <#someChannel> <@botName> <@botMention> <@targetName> <@targetMention>"
            let exp =
                [
                    Text "<:someEmoji:12345> text "
                    CustomMention (Author, Name)
                    Text " ";
                    CustomMention (Author, Mention)
                    Text " "
                    Text "<#someChannel> ";
                    CustomMention (Bot, Name)
                    Text " "
                    CustomMention (Bot, Mention)
                    Text " "
                    CustomMention (Target, Name)
                    Text " ";
                    CustomMention (Target, Mention)
                ]
                |> Ok

            Assert.Equal("", exp, act)
    ]

[<Tests>]
let ``CustomCommand.MessageTemplate.Message.toString`` =
    testList "CustomCommand.MessageTemplate.Message.toString" [
        testCase "base" <| fun () ->
            let act =
                [
                    Text "<:someEmoji:12345> text "
                    CustomMention (Author, Name)
                    Text " ";
                    CustomMention (Author, Mention)
                    Text " "
                    Text "<#someChannel> ";
                    CustomMention (Bot, Name)
                    Text " "
                    CustomMention (Bot, Mention)
                    Text " "
                    CustomMention (Target, Name)
                    Text " ";
                    CustomMention (Target, Mention)
                ]
                |> Message.toString

            let exp =
                "<:someEmoji:12345> text <@authorName> <@authorMention> <#someChannel> <@botName> <@botMention> <@targetName> <@targetMention>"

            Assert.Equal("", exp, act)
    ]
