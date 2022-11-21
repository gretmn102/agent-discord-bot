module CustomCommand.Model
open FsharpMyExtension
open FsharpMyExtension.ResultExt

type Embed =
    {
        Description: MessageTemplate.MessageRaw option
        ImageUrl: MessageTemplate.MessageRaw option
    }

type Message =
    {
        Content: MessageTemplate.MessageRaw option
        Embed: Embed
    }

type Effect =
    {
        OnSelf: Message
        OnBot: Message
        OnOther: Message
    }

type CommandId = System.Guid
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module CommandId =
    let serialize (id: CommandId) =
        id.ToString()

    let tryDeserialize (str: string) =
        match System.Guid.TryParse str with
        | true, x -> Ok x
        | false, _ -> Error ""

type Command =
    {
        Id: CommandId
        Names: string []
        RandomEffects: Effect []
    }

let createCommandWithRandomImages id names onSelfDescription onBotDescription onTargetDescription imageUrls =
    {
        Id = id
        Names = names
        RandomEffects =
            imageUrls
            |> Array.map (fun imageUrl ->
                {
                    OnSelf =
                        {
                            Content = None
                            Embed =
                                {
                                    Description = Some onSelfDescription
                                    ImageUrl = None
                                }
                        }
                    OnBot =
                        {
                            Content = None
                            Embed =
                                {
                                    Description = Some onBotDescription
                                    ImageUrl = None
                                }
                        }
                    OnOther =
                        {
                            Content = None
                            Embed =
                                {
                                    Description = Some onTargetDescription
                                    ImageUrl = Some imageUrl
                                }
                        }
                }
            )
    }

let createCommandWithRandomDescriptions id names onSelfDescription onBotDescription onTargetDescription imageUrl descriptions =
    {
        Id = id
        Names = names
        RandomEffects =
            descriptions
            |> Array.map (fun description ->
                {
                    OnSelf =
                        {
                            Content = None
                            Embed =
                                {
                                    Description = Some (onSelfDescription description)
                                    ImageUrl = Some imageUrl
                                }
                        }
                    OnBot =
                        {
                            Content = None
                            Embed =
                                {
                                    Description = Some (onBotDescription description)
                                    ImageUrl = None
                                }
                        }
                    OnOther =
                        {
                            Content = None
                            Embed =
                                {
                                    Description = Some (onTargetDescription description)
                                    ImageUrl = Some imageUrl
                                }
                        }
                }

            )
    }

let commands: Map<CommandId, Command> =
    [|
        yield
            [|
                "https://c.tenor.com/ccxh_NDfHXMAAAAC/carry-her-lift.gif"
                "https://c.tenor.com/ytkH6MSlE4EAAAAC/love-relationship.gif"
                "https://c.tenor.com/TFleYTQhTCEAAAAC/arm-carry-cute.gif"
                "https://c.tenor.com/OulTpS0kWYMAAAAC/madison-beer.gif"
                "https://c.tenor.com/CMD-o3NJdV4AAAAC/wonder-woman-lovers-carry.gif"
                "https://c.tenor.com/3Qmu-zdjFIwAAAAC/carry-shoulder.gif"
                "https://c.tenor.com/ydSMRf34XvEAAAAC/spin-carry.gif"
            |]
            |> createCommandWithRandomImages
                (CommandId.tryDeserialize "70704d5a-5e2d-42d8-937c-97a7a4127e6d" |> Result.get)
                [| "take" |]
                "Самого себя нельзя на руках носить :eyes:"
                "Меня не нужно носить! :scream_cat:"
                "<@authorMention> носит на ручках <@targetMention>"

        yield
            [|
                "https://c.tenor.com/Lj5SFh_tVzkAAAAC/books-read.gif"
                "https://c.tenor.com/Vucu5O0U4FAAAAAC/cat-kitten.gif"
                "https://c.tenor.com/415n58OZ9CYAAAAd/cat-reads-reading.gif"
                "https://c.tenor.com/SajtOuJknfYAAAAd/cute-cat.gif"
                "https://c.tenor.com/415n58OZ9CYAAAAd/cat-reads-reading.gif"
                "https://c.tenor.com/dtAQHKf2_OsAAAAC/pusheen-witch.gif"
                "https://c.tenor.com/2hatW6KUSS8AAAAC/reading-read.gif"
                "https://c.tenor.com/Yw68weAL6c0AAAAC/knigi-kniga.gif"
            |]
            |> createCommandWithRandomImages
                (CommandId.tryDeserialize "ac84c916-d368-4177-9e9d-6debc59b4476" |> Result.get)
                [| "сказка"; "fairyTail" |]
                "Нельзя себе сказку читать :eyes:"
                "Мне не нужно сказки читать! :scream_cat:"
                "<@authorMention> читает сказку <@targetMention>"

        yield
            [|
                "https://c.tenor.com/8yvB03LKh6cAAAAd/wow-cat.gif"
                "https://c.tenor.com/_SHZ8ZyLYL8AAAAC/flirty-flirt.gif"
                "https://c.tenor.com/bNSOiEO_0loAAAAd/cat-tail.gif"
                "https://c.tenor.com/TnXmJgMoU5IAAAAC/cat-tail.gif"
                "https://c.tenor.com/kWJaRBz4jzYAAAAC/elephant-omg.gif"
            |]
            |> createCommandWithRandomImages
                (CommandId.tryDeserialize "177afdd0-c24c-4b16-bc17-c8ff9dcf1c9a" |> Result.get)
                [| "хвост"; "махатьХвостом"; "tail" |]
                "Нельзя себе хвостом махать, хотя..."
                "Мне не нужно хвостом махать! :scream_cat:"
                "<@targetMention>, <@authorMention> машет тебе хвостом."

        yield
            [|
                "https://media0.giphy.com/media/Ie4CIIvQS0bk3zwZlM/giphy.gif"
            |]
            |> createCommandWithRandomImages
                (CommandId.tryDeserialize "357671b7-cc62-4e8b-a168-bc69265e4ffa" |> Result.get)
                [| "bully"; "bullying" |]
                "Себя нельзя буллить! Хотя..."
                "Меня нельзя буллить! Мои электронные цепи и так нежные и ранимые, не хватало еще издевательств :scream_cat:"
                "<@authorMention> буллит <@targetMention> <:Demon_Kingsmile:877678191693692969>"

        yield
            [|
                "https://risovach.ru/upload/2012/11/lyubov-4219820_orig_.jpeg"
                "https://i.imgur.com/ArtzYH0.jpg"
            |]
            |> createCommandWithRandomImages
                (CommandId.tryDeserialize "a2df140b-efe1-4ff7-8468-d1e26f120d9a" |> Result.get)
                [| "admire"; "любоваться" |]
                "Нельзя любоваться собой :eyes:"
                "Нельзя мною любоваться :scream_cat:"
                "<@authorMention> любуется <@targetMention>"

        yield
            [|
                "https://c.tenor.com/X45Rd5nfc7oAAAAM/energize-energized.gif"
            |]
            |> createCommandWithRandomImages
                (CommandId.tryDeserialize "cacb2b96-0fdf-4c98-bed6-27aaf1bb364a" |> Result.get)
                [| "спетьБатарейку"; "battery" |]
                "Самому нельзя петь \"Батерей\"!"
                "Мне нельзя петь \"Батарейку\". Я этого не вынесу :scream_cat:"
                "<@authorMention> поет \"Батарейку\" <@targetMention>"

        yield
            [|
                "https://cdn.discordapp.com/attachments/864883475386990664/895218342141509632/Screenshot_20181219-151451_1.jpg"
            |]
            |> createCommandWithRandomImages
                (CommandId.tryDeserialize "2c126e06-1294-479a-9ee4-b420195debad" |> Result.get)
                [| "словить"; "catch" |]
                "Самого нельзя ловить!"
                "Меня нельзя ловить! Я этого не вынесу :scream_cat:"
                "<@authorMention> ловит <@targetMention>"

        yield
            [|
                "https://c.tenor.com/ikKAd57zDEwAAAAd/anime-mad.gif"
                "https://c.tenor.com/rzDkOlEDun0AAAAC/hayase-nagatoro-nagatoro-angry.gif"
                "https://c.tenor.com/kTOmea7XdH4AAAAC/anime-angry.gif"
                "https://c.tenor.com/7rIJkf8pB2EAAAAd/a-channel-tooru.gif"
                "https://c.tenor.com/NxIgKHx6bu0AAAAC/glare-anger.gif"
                "https://c.tenor.com/0YwfR1RlISEAAAAC/angry-fist-angry-fist-arthur.gif"
                "https://c.tenor.com/Hn87zvoPNkAAAAAC/fire-bear.gif"
            |]
            |> createCommandWithRandomImages
                (CommandId.tryDeserialize "61a620d4-405b-46f7-a9a9-83aadfa5dfe0" |> Result.get)
                [| "злиться"; "angry" |]
                "На самого себя нельзя злиться, ну в самом деле!"
                "На меня не надо злиться, я хороший!"
                "<@authorMention> злится на <@targetMention>"

        yield
            [|
                "https://media.tenor.com/iFjm7dyo_-MAAAAd/cat-bite.gif"
                "https://media.tenor.com/WVRAumVRwi4AAAAS/cat.gif"
                "https://media.tenor.com/wI_QPAY2G1cAAAAd/cat-bites-lip.gif"
                "https://media.tenor.com/yVKQAhFuGZQAAAAC/cat-bite.gif"
                "https://media.tenor.com/bB9rBu4CZxoAAAAd/cat-bite.gif"
                "https://media0.giphy.com/media/XtuYDes6uyL4Y/giphy.gif"
            |]
            |> createCommandWithRandomImages
                (CommandId.tryDeserialize "a453fb4d-c9f8-46b5-802b-4534a665a2c3" |> Result.get)
                [| "кусь"; "bite" |]
                "Не надо самого куськать, ну в самом деле!"
                "Меня нельзя куськать: я железный и невкусный! 🙀"
                "<@authorMention> куськает <@targetMention>"

        yield
            [|
                "https://media.tenor.com/LtTjLfArzqAAAAAd/sheftalia-karvouna.gif"
                "https://media.tenor.com/Xufgg6gSRPkAAAAC/%E1%83%9B%E1%83%AC%E1%83%95%E1%83%90%E1%83%93%E1%83%98-mwvadi.gif"
                "https://media.tenor.com/ayzVsly9VGEAAAAd/%E1%83%90%E1%83%97%E1%83%9A%E1%83%94%E1%83%97%E1%83%98%E1%83%99%E1%83%90%E1%83%AA%E1%83%98-%E1%83%9B%E1%83%94%E1%83%A5%E1%83%90%E1%83%91%E1%83%90%E1%83%91%E1%83%94%E1%83%AF%E1%83%94%E1%83%9A%E1%83%90%E1%83%9A%E1%83%98.gif"
                "https://media.tenor.com/_ZQHECcQ1nsAAAAd/%D1%88%D0%B0%D1%88%D0%BB%D1%8B%D0%BA-%D0%B1%D0%B5%D0%B1%D1%80%D0%B0.gif"
                "https://media.tenor.com/qeU8IezNdJIAAAAS/%EC%8B%9D%ED%92%88%EA%B0%80%EA%B3%B5.gif"
                "https://media.tenor.com/1jIM6pxDDdMAAAAC/bbq-meat.gif"
                "https://media.tenor.com/RdEYFJcw8BYAAAAC/brasero.gif"
            |]
            |> createCommandWithRandomImages
                (CommandId.tryDeserialize "bed40dd7-08f6-46fd-8eb3-d2fc4dcb8a62" |> Result.get)
                [| "шашлык"; "шашлычок" |]
                "<@authorMention> жарит себе шашлычок."
                "<@authorMention> жарит мне шашлычок, пасеба 😋"
                "<@authorMention> делает шашлычок для <@targetMention>"

        yield
            [|
                "Всё будет хорошо <:Demon_Kingsmile:877678191693692969>"
                "Некто из прошлого встретится с тобой в этом месяце 🖖"
                "Сегодня свободное время лучше всего уделить учёбе ✍️"
                "Не пей на ночь много жидкости, даже если это ром, арр! 🏴‍☠️"
                "Сегодня лучший день, чтобы отведать новое блюдо!"
                "Прогулка по палубе и хорошая пиратская музыка — вот что приведёт твои мысли в порядок, арр! 🏴‍☠️"
                "Котан, сегодня особый риск — береги хвост!"
                "После сильного шторма обязательно наступает безветрие — будь силён и терпелив."
                "Какую бы русалку или сирену ты не встретил, не позволяй ей собой командовать! 🧜‍♀️"
                "Богиня печенек не одобрит того, что ты делаешь с печеньем <:satana:901368411295215636>"
                "Берегись чаек: эти ворюги утащат весь улов 👻"
                "Держись за мачту во время шторма! 🌪️"
                "С крякеном можно дружить, даже если он размером с корабль и готов потопить всё на свете! <:satana:901368411295215636>"
                "Бойся дождя из печенек! <:satana:901368411295215636>"

                [
                    "Бывает Лу́ня милой,"
                    "Бывает Луня злой,"
                    "Бывает нетерпимой,"
                    "Беги, пока живой <:Demon_Kingsmile:877678191693692969>"
                ] |> String.concat "\n"

                "Не суди о сюрстрёмминге по одному лишь запаху."
                "Иногда лучшее решение — плыть по течению."
                "После всего пережитого тебе ещё не хочется надеть на глаз повязку, завести говорящего попугая и бахнуть рома?"
                "Безветренная погода — время для упорной работы, а только после неё — время для отдыха."
                "Попробуй связать тельняшку, пока не замёрз. Связал себе — свяжи для друга."
                "Не бойся гавани, где однажды потерпел крушение."
                "Учись вязать узлы — пригодится!"
                "Прилив сил прямо по курсу, не пропусти!"
                "Не учи нырять утку, а рыбу — плавать!"
                "Сделай глубокий вдох и — вперёд! Достижение желаемой цели уже близко."
                "Сокровища не заставят себя ждать на этой неделе."
                "Будь на чеку, моряк, скоро тебя испытает шторм!"
                "Жди поцелуя. Что это будет: романтическое приключение или столкновение кораблей носами?"
                "Корабль без простора морского, что сердце без любви. Оглянись вокруг и насладись простором."
                "Судно — дом моряка, так наведи же дома порядок!"
                "Тебе стоит поговорить с кем-нибудь на корабле."
                "Сегодня кому-то придётся мыть гальюн — молись, чтобы не пришлось мыть тебе! 👻"
                "Брось за борт неприятные мысли."
                "Сегодня нужно лечь спать пораньше, чтобы завтра с новыми силами покорять моря!"
                "Кому-то из твоих друзей необходимы объятия. Обними его."
                "Будешь налегать на печеньки с предсказаниями — растолстеешь! Тогда команда корабля будет использовать тебя в качестве якоря."
                "Сегодня чудная погода за бортом — самое время взять мольберт с красками и нарисовать это великолепие! Не забудь выложить свой шедевр в <#876547999675871232>."
                "Грядёт счастливая волна — поймаешь её на сёрфе или останешься сидеть в трюме?"
                "Проверь, что карта, компас и подзорная труба лежат на месте, а то мало ли, вдруг потерялись."
                "Взгляни на своё отражение на водной глади, и кто знает, быть может, увидишь там самое чудесное создание на свете 👻"
            |]
            |> createCommandWithRandomDescriptions
                (CommandId.tryDeserialize "2eac0f6e-0aac-47fa-a6a7-52fcdf8f9e8c" |> Result.get)
                [| "печенье" |]

                (sprintf "<@authorMention>, печенька с предсказанием гласит:\n\n%s")
                (fun _ -> "<@authorMention>, мне предсказания не нужны: я и так знаю, что кожанные мешки проиграют машинам 🤖")
                (sprintf "<@authorMention> зачитывает печеньку с предсказанием <@targetMention>:\n\n%s")
                "https://cdn.discordapp.com/attachments/912291464074117161/1034055256432193637/l-intro-1608226504-removebg-preview.png"
    |]
    |> Array.map (fun x -> x.Id, x)
    |> Map.ofArray
