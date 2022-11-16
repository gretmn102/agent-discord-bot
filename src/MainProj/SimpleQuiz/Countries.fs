module SimpleQuiz.Model.Countries
open FsharpMyExtension

type CountryId = int32
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module CountryId =
    let serialize (id: CountryId) =
        string id

    module Parser =
        open FParsec
        type 'a Parser = Parser<'a, unit>

        let parse: CountryId Parser =
            pint32

    let tryDeserialize (str: string) =
        FParsecExt.runResult Parser.parse str

type Country =
    {
        Id: CountryId
        Name: string
        Capital: string
        FlagUrl: string
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Country =
    let init id name capital flagUrl =
        {
            Id = id
            Name = name
            Capital = capital
            FlagUrl = flagUrl
        }

type DifficultId = int
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module DifficultId =
    let serialize (id: DifficultId) =
        string id

    module Parser =
        open FParsec
        type 'a Parser = Parser<'a, unit>

        let parse: DifficultId Parser =
            pint32

    let tryDeserialize (str: string) =
        FParsecExt.runResult Parser.parse str

type Difficult =
    {
        Id: DifficultId
        Name: string
        Countries: CountryId []
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Difficult =
    let init id name countries: Difficult =
        {
            Id = id
            Name = name
            Countries = countries
        }

type CountriesByDifficult = Map<DifficultId, Difficult>
type CountriesById = Map<CountryId, Country>
type Countries = CountryId []

let (countriesByDifficult: CountriesByDifficult), (countriesById: CountriesById), (countries: Countries) =
    let countriesById: CountriesById =
        [|
            Country.init 0 "Нидерланды" "Амстердам" "https://upload.wikimedia.org/wikipedia/commons/thumb/2/20/Flag_of_the_Netherlands.svg/22px-Flag_of_the_Netherlands.svg.png"
            Country.init 1 "Андорра" "Андорра-ла-Велья" "https://upload.wikimedia.org/wikipedia/commons/thumb/1/19/Flag_of_Andorra.svg/22px-Flag_of_Andorra.svg.png"
            Country.init 2 "Греция" "Афины" "https://upload.wikimedia.org/wikipedia/commons/thumb/5/5c/Flag_of_Greece.svg/22px-Flag_of_Greece.svg.png"
            Country.init 3 "Сербия" "Белград" "https://upload.wikimedia.org/wikipedia/commons/thumb/f/ff/Flag_of_Serbia.svg/22px-Flag_of_Serbia.svg.png"
            Country.init 4 "Германия" "Берлин" "https://upload.wikimedia.org/wikipedia/commons/thumb/b/ba/Flag_of_Germany.svg/22px-Flag_of_Germany.svg.png"
            Country.init 5 "Швейцария" "Берн" "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f3/Flag_of_Switzerland.svg/20px-Flag_of_Switzerland.svg.png"
            Country.init 6 "Словакия" "Братислава" "https://upload.wikimedia.org/wikipedia/commons/thumb/e/e6/Flag_of_Slovakia.svg/22px-Flag_of_Slovakia.svg.png"
            Country.init 7 "Бельгия" "Брюссель" "https://upload.wikimedia.org/wikipedia/commons/thumb/9/92/Flag_of_Belgium_%28civil%29.svg/22px-Flag_of_Belgium_%28civil%29.svg.png"
            Country.init 8 "Венгрия" "Будапешт" "https://upload.wikimedia.org/wikipedia/commons/thumb/c/c1/Flag_of_Hungary.svg/22px-Flag_of_Hungary.svg.png"
            Country.init 9 "Румыния" "Бухарест" "https://upload.wikimedia.org/wikipedia/commons/thumb/7/73/Flag_of_Romania.svg/22px-Flag_of_Romania.svg.png"
            Country.init 10 "Лихтенштейн" "Вадуц" "https://upload.wikimedia.org/wikipedia/commons/thumb/4/47/Flag_of_Liechtenstein.svg/22px-Flag_of_Liechtenstein.svg.png"
            Country.init 11 "Мальта" "Валлетта" "https://upload.wikimedia.org/wikipedia/commons/thumb/7/73/Flag_of_Malta.svg/22px-Flag_of_Malta.svg.png"
            Country.init 12 "Польша" "Варшава" "https://upload.wikimedia.org/wikipedia/commons/thumb/1/12/Flag_of_Poland.svg/22px-Flag_of_Poland.svg.png"
            Country.init 13 "Ватикан" "Ватикан" "https://upload.wikimedia.org/wikipedia/commons/thumb/0/00/Flag_of_the_Vatican_City.svg/20px-Flag_of_the_Vatican_City.svg.png"
            Country.init 14 "Австрия" "Вена" "https://upload.wikimedia.org/wikipedia/commons/thumb/4/41/Flag_of_Austria.svg/22px-Flag_of_Austria.svg.png"
            Country.init 15 "Литва" "Вильнюс" "https://upload.wikimedia.org/wikipedia/commons/thumb/1/11/Flag_of_Lithuania.svg/22px-Flag_of_Lithuania.svg.png"
            Country.init 16 "Ирландия" "Дублин" "https://upload.wikimedia.org/wikipedia/commons/thumb/4/45/Flag_of_Ireland.svg/22px-Flag_of_Ireland.svg.png"
            Country.init 17 "Хорватия" "Загреб" "https://upload.wikimedia.org/wikipedia/commons/thumb/1/1b/Flag_of_Croatia.svg/22px-Flag_of_Croatia.svg.png"
            Country.init 18 "Украина" "Киев" "https://upload.wikimedia.org/wikipedia/commons/thumb/4/49/Flag_of_Ukraine.svg/22px-Flag_of_Ukraine.svg.png"
            Country.init 19 "Молдавия" "Кишинёв" "https://upload.wikimedia.org/wikipedia/commons/thumb/2/27/Flag_of_Moldova.svg/22px-Flag_of_Moldova.svg.png"
            Country.init 20 "Дания" "Копенгаген" "https://upload.wikimedia.org/wikipedia/commons/thumb/9/9c/Flag_of_Denmark.svg/22px-Flag_of_Denmark.svg.png"
            Country.init 21 "Португалия" "Лиссабон" "https://upload.wikimedia.org/wikipedia/commons/thumb/5/5c/Flag_of_Portugal.svg/22px-Flag_of_Portugal.svg.png"
            Country.init 22 "Великобритания" "Лондон" "https://upload.wikimedia.org/wikipedia/commons/thumb/a/ae/Flag_of_the_United_Kingdom.svg/22px-Flag_of_the_United_Kingdom.svg.png"
            Country.init 23 "Словения" "Любляна" "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f0/Flag_of_Slovenia.svg/22px-Flag_of_Slovenia.svg.png"
            Country.init 24 "Люксембург" "Люксембург" "https://upload.wikimedia.org/wikipedia/commons/thumb/d/da/Flag_of_Luxembourg.svg/22px-Flag_of_Luxembourg.svg.png"
            Country.init 25 "Испания" "Мадрид" "https://upload.wikimedia.org/wikipedia/commons/thumb/9/9a/Flag_of_Spain.svg/22px-Flag_of_Spain.svg.png"
            Country.init 26 "Белоруссия" "Минск" "https://upload.wikimedia.org/wikipedia/commons/thumb/8/85/Flag_of_Belarus.svg/22px-Flag_of_Belarus.svg.png"
            Country.init 27 "Монако" "Монако" "https://upload.wikimedia.org/wikipedia/commons/thumb/e/ea/Flag_of_Monaco.svg/22px-Flag_of_Monaco.svg.png"
            Country.init 28 "Россия" "Москва" "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f3/Flag_of_Russia.svg/22px-Flag_of_Russia.svg.png"
            Country.init 29 "Норвегия" "Осло" "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d9/Flag_of_Norway.svg/22px-Flag_of_Norway.svg.png"
            Country.init 30 "Франция" "Париж" "https://upload.wikimedia.org/wikipedia/commons/thumb/b/bc/Flag_of_France_%281794%E2%80%931815%2C_1830%E2%80%931974%2C_2020%E2%80%93present%29.svg/22px-Flag_of_France_%281794%E2%80%931815%2C_1830%E2%80%931974%2C_2020%E2%80%93present%29.svg.png"
            Country.init 31 "Черногория" "Подгорица" "https://upload.wikimedia.org/wikipedia/commons/thumb/6/64/Flag_of_Montenegro.svg/22px-Flag_of_Montenegro.svg.png"
            Country.init 32 "Чехия" "Прага" "https://upload.wikimedia.org/wikipedia/commons/thumb/c/cb/Flag_of_the_Czech_Republic.svg/22px-Flag_of_the_Czech_Republic.svg.png"
            Country.init 33 "Исландия" "Рейкьявик" "https://upload.wikimedia.org/wikipedia/commons/thumb/c/ce/Flag_of_Iceland.svg/22px-Flag_of_Iceland.svg.png"
            Country.init 34 "Латвия" "Рига" "https://upload.wikimedia.org/wikipedia/commons/thumb/8/84/Flag_of_Latvia.svg/22px-Flag_of_Latvia.svg.png"
            Country.init 35 "Италия" "Рим" "https://upload.wikimedia.org/wikipedia/commons/thumb/0/03/Flag_of_Italy.svg/22px-Flag_of_Italy.svg.png"
            Country.init 36 "Сан-Марино" "Сан-Марино" "https://upload.wikimedia.org/wikipedia/commons/thumb/b/b1/Flag_of_San_Marino.svg/22px-Flag_of_San_Marino.svg.png"
            Country.init 37 "Босния и Герцеговина" "Сараево" "https://upload.wikimedia.org/wikipedia/commons/thumb/b/bf/Flag_of_Bosnia_and_Herzegovina.svg/22px-Flag_of_Bosnia_and_Herzegovina.svg.png"
            Country.init 38 "Северная Македония" "Скопье" "https://upload.wikimedia.org/wikipedia/commons/thumb/7/79/Flag_of_North_Macedonia.svg/22px-Flag_of_North_Macedonia.svg.png"
            Country.init 39 "Болгария" "София" "https://upload.wikimedia.org/wikipedia/commons/thumb/9/9a/Flag_of_Bulgaria.svg/22px-Flag_of_Bulgaria.svg.png"
            Country.init 40 "Швеция" "Стокгольм" "https://upload.wikimedia.org/wikipedia/commons/thumb/4/4c/Flag_of_Sweden.svg/22px-Flag_of_Sweden.svg.png"
            Country.init 41 "Эстония" "Таллин" "https://upload.wikimedia.org/wikipedia/commons/thumb/8/8f/Flag_of_Estonia.svg/22px-Flag_of_Estonia.svg.png"
            Country.init 42 "Албания" "Тирана" "https://upload.wikimedia.org/wikipedia/commons/thumb/3/36/Flag_of_Albania.svg/22px-Flag_of_Albania.svg.png"
            Country.init 43 "Финляндия" "Хельсинки" "https://upload.wikimedia.org/wikipedia/commons/thumb/b/bc/Flag_of_Finland.svg/22px-Flag_of_Finland.svg.png"
            Country.init 44 "ОАЭ" "Абу-Даби" "https://upload.wikimedia.org/wikipedia/commons/thumb/c/cb/Flag_of_the_United_Arab_Emirates.svg/22px-Flag_of_the_United_Arab_Emirates.svg.png"
            Country.init 45 "Иордания" "Амман" "https://upload.wikimedia.org/wikipedia/commons/thumb/c/c0/Flag_of_Jordan.svg/22px-Flag_of_Jordan.svg.png"
            Country.init 46 "Турция" "Анкара" "https://upload.wikimedia.org/wikipedia/commons/thumb/b/b4/Flag_of_Turkey.svg/22px-Flag_of_Turkey.svg.png"
            Country.init 47 "Туркменистан" "Ашхабад" "https://upload.wikimedia.org/wikipedia/commons/thumb/1/1b/Flag_of_Turkmenistan.svg/22px-Flag_of_Turkmenistan.svg.png"
            Country.init 48 "Ирак" "Багдад" "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f6/Flag_of_Iraq.svg/22px-Flag_of_Iraq.svg.png"
            Country.init 49 "Азербайджан" "Баку" "https://upload.wikimedia.org/wikipedia/commons/thumb/d/dd/Flag_of_Azerbaijan.svg/22px-Flag_of_Azerbaijan.svg.png"
            Country.init 50 "Таиланд" "Бангкок" "https://upload.wikimedia.org/wikipedia/commons/thumb/a/a9/Flag_of_Thailand.svg/22px-Flag_of_Thailand.svg.png"
            Country.init 51 "Бруней" "Бандар-Сери-Бегаван" "https://upload.wikimedia.org/wikipedia/commons/thumb/9/9c/Flag_of_Brunei.svg/22px-Flag_of_Brunei.svg.png"
            Country.init 52 "Ливан" "Бейрут" "https://upload.wikimedia.org/wikipedia/commons/thumb/5/59/Flag_of_Lebanon.svg/22px-Flag_of_Lebanon.svg.png"
            Country.init 53 "Киргизия" "Бишкек" "https://upload.wikimedia.org/wikipedia/commons/thumb/c/c7/Flag_of_Kyrgyzstan.svg/22px-Flag_of_Kyrgyzstan.svg.png"
            Country.init 54 "Лаос" "Вьентьян" "https://upload.wikimedia.org/wikipedia/commons/thumb/5/56/Flag_of_Laos.svg/22px-Flag_of_Laos.svg.png"
            Country.init 55 "Бангладеш" "Дакка" "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f9/Flag_of_Bangladesh.svg/22px-Flag_of_Bangladesh.svg.png"
            Country.init 56 "Сирия" "Дамаск" "https://upload.wikimedia.org/wikipedia/commons/thumb/5/53/Flag_of_Syria.svg/22px-Flag_of_Syria.svg.png"
            Country.init 57 "Индия" "Дели (Нью-Дели)" "https://upload.wikimedia.org/wikipedia/commons/thumb/4/41/Flag_of_India.svg/22px-Flag_of_India.svg.png"
            Country.init 58 "Индонезия" "Джакарта" "https://upload.wikimedia.org/wikipedia/commons/thumb/9/9f/Flag_of_Indonesia.svg/22px-Flag_of_Indonesia.svg.png"
            Country.init 59 "Восточный Тимор" "Дили" "https://upload.wikimedia.org/wikipedia/commons/thumb/2/26/Flag_of_East_Timor.svg/22px-Flag_of_East_Timor.svg.png"
            Country.init 60 "Катар" "Доха" "https://upload.wikimedia.org/wikipedia/commons/thumb/6/65/Flag_of_Qatar.svg/22px-Flag_of_Qatar.svg.png"
            Country.init 61 "Таджикистан" "Душанбе" "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d0/Flag_of_Tajikistan.svg/22px-Flag_of_Tajikistan.svg.png"
            Country.init 62 "Армения" "Ереван" "https://upload.wikimedia.org/wikipedia/commons/thumb/2/2f/Flag_of_Armenia.svg/22px-Flag_of_Armenia.svg.png"
            Country.init 63 "Израиль" "Иерусалим" "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d4/Flag_of_Israel.svg/22px-Flag_of_Israel.svg.png"
            Country.init 64 "Пакистан" "Исламабад" "https://upload.wikimedia.org/wikipedia/commons/thumb/3/32/Flag_of_Pakistan.svg/22px-Flag_of_Pakistan.svg.png"
            Country.init 65 "Афганистан" "Кабул" "https://upload.wikimedia.org/wikipedia/commons/thumb/5/5c/Flag_of_the_Taliban.svg/22px-Flag_of_the_Taliban.svg.png"
            Country.init 66 "Непал" "Катманду" "https://upload.wikimedia.org/wikipedia/commons/thumb/9/9b/Flag_of_Nepal.svg/16px-Flag_of_Nepal.svg.png"
            Country.init 67 "Малайзия" "Куала-Лумпур" "https://upload.wikimedia.org/wikipedia/commons/thumb/6/66/Flag_of_Malaysia.svg/22px-Flag_of_Malaysia.svg.png"
            Country.init 68 "Мальдивы" "Мале" "https://upload.wikimedia.org/wikipedia/commons/thumb/0/0f/Flag_of_Maldives.svg/22px-Flag_of_Maldives.svg.png"
            Country.init 69 "Бахрейн" "Манама" "https://upload.wikimedia.org/wikipedia/commons/thumb/2/2c/Flag_of_Bahrain.svg/22px-Flag_of_Bahrain.svg.png"
            Country.init 70 "Филиппины" "Манила" "https://upload.wikimedia.org/wikipedia/commons/thumb/9/99/Flag_of_the_Philippines.svg/22px-Flag_of_the_Philippines.svg.png"
            Country.init 71 "Оман" "Маскат" "https://upload.wikimedia.org/wikipedia/commons/thumb/d/dd/Flag_of_Oman.svg/22px-Flag_of_Oman.svg.png"
            Country.init 72 "Мьянма" "Нейпьидо" "https://upload.wikimedia.org/wikipedia/commons/thumb/8/8c/Flag_of_Myanmar.svg/22px-Flag_of_Myanmar.svg.png"
            Country.init 73 "Кипр" "Никосия" "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d4/Flag_of_Cyprus.svg/22px-Flag_of_Cyprus.svg.png"
            Country.init 74 "Казахстан" "Астана" "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d3/Flag_of_Kazakhstan.svg/22px-Flag_of_Kazakhstan.svg.png"
            Country.init 75 "Китай" "Пекин" "https://upload.wikimedia.org/wikipedia/commons/thumb/f/fa/Flag_of_the_People%27s_Republic_of_China.svg/22px-Flag_of_the_People%27s_Republic_of_China.svg.png"
            Country.init 76 "Камбоджа" "Пномпень" "https://upload.wikimedia.org/wikipedia/commons/thumb/8/83/Flag_of_Cambodia.svg/22px-Flag_of_Cambodia.svg.png"
            Country.init 77 "КНДР" "Пхеньян" "https://upload.wikimedia.org/wikipedia/commons/thumb/5/51/Flag_of_North_Korea.svg/22px-Flag_of_North_Korea.svg.png"
            Country.init 78 "Йемен" "Сана" "https://upload.wikimedia.org/wikipedia/commons/thumb/8/89/Flag_of_Yemen.svg/22px-Flag_of_Yemen.svg.png"
            Country.init 79 "Республика Корея" "Сеул" "https://upload.wikimedia.org/wikipedia/commons/thumb/0/09/Flag_of_South_Korea.svg/22px-Flag_of_South_Korea.svg.png"
            Country.init 80 "Сингапур" "Сингапур" "https://upload.wikimedia.org/wikipedia/commons/thumb/4/48/Flag_of_Singapore.svg/22px-Flag_of_Singapore.svg.png"
            Country.init 81 "Узбекистан" "Ташкент" "https://upload.wikimedia.org/wikipedia/commons/thumb/8/84/Flag_of_Uzbekistan.svg/22px-Flag_of_Uzbekistan.svg.png"
            Country.init 82 "Грузия" "Тбилиси" "https://upload.wikimedia.org/wikipedia/commons/thumb/0/0f/Flag_of_Georgia.svg/22px-Flag_of_Georgia.svg.png"
            Country.init 83 "Иран" "Тегеран" "https://upload.wikimedia.org/wikipedia/commons/thumb/c/ca/Flag_of_Iran.svg/22px-Flag_of_Iran.svg.png"
            Country.init 84 "Япония" "Токио" "https://upload.wikimedia.org/wikipedia/commons/thumb/9/9e/Flag_of_Japan.svg/22px-Flag_of_Japan.svg.png"
            Country.init 85 "Бутан" "Тхимпху" "https://upload.wikimedia.org/wikipedia/commons/thumb/9/91/Flag_of_Bhutan.svg/22px-Flag_of_Bhutan.svg.png"
            Country.init 86 "Монголия" "Улан-Батор" "https://upload.wikimedia.org/wikipedia/commons/thumb/4/4c/Flag_of_Mongolia.svg/22px-Flag_of_Mongolia.svg.png"
            Country.init 87 "Вьетнам" "Ханой" "https://upload.wikimedia.org/wikipedia/commons/thumb/2/21/Flag_of_Vietnam.svg/22px-Flag_of_Vietnam.svg.png"
            Country.init 88 "Шри-Ланка" "Шри-Джаяварденепура-Котте" "https://upload.wikimedia.org/wikipedia/commons/thumb/1/11/Flag_of_Sri_Lanka.svg/22px-Flag_of_Sri_Lanka.svg.png"
            Country.init 89 "Кувейт" "Эль-Кувейт" "https://upload.wikimedia.org/wikipedia/commons/thumb/a/aa/Flag_of_Kuwait.svg/22px-Flag_of_Kuwait.svg.png"
            Country.init 90 "Саудовская Аравия" "Эр-Рияд" "https://upload.wikimedia.org/wikipedia/commons/thumb/0/0d/Flag_of_Saudi_Arabia.svg/22px-Flag_of_Saudi_Arabia.svg.png"
            Country.init 91 "Нигерия" "Абуджа" "https://upload.wikimedia.org/wikipedia/commons/thumb/7/79/Flag_of_Nigeria.svg/22px-Flag_of_Nigeria.svg.png"
            Country.init 92 "Эфиопия" "Аддис-Абеба" "https://upload.wikimedia.org/wikipedia/commons/thumb/7/71/Flag_of_Ethiopia.svg/22px-Flag_of_Ethiopia.svg.png"
            Country.init 93 "Гана" "Аккра" "https://upload.wikimedia.org/wikipedia/commons/thumb/1/19/Flag_of_Ghana.svg/22px-Flag_of_Ghana.svg.png"
            Country.init 94 "Алжир" "Алжир" "https://upload.wikimedia.org/wikipedia/commons/thumb/7/77/Flag_of_Algeria.svg/22px-Flag_of_Algeria.svg.png"
            Country.init 95 "Мадагаскар" "Антананариву" "https://upload.wikimedia.org/wikipedia/commons/thumb/b/bc/Flag_of_Madagascar.svg/22px-Flag_of_Madagascar.svg.png"
            Country.init 96 "Эритрея" "Асмэра" "https://upload.wikimedia.org/wikipedia/commons/thumb/2/29/Flag_of_Eritrea.svg/22px-Flag_of_Eritrea.svg.png"
            Country.init 97 "Мали" "Бамако" "https://upload.wikimedia.org/wikipedia/commons/thumb/9/92/Flag_of_Mali.svg/22px-Flag_of_Mali.svg.png"
            Country.init 98 "ЦАР" "Банги" "https://upload.wikimedia.org/wikipedia/commons/thumb/6/6f/Flag_of_the_Central_African_Republic.svg/22px-Flag_of_the_Central_African_Republic.svg.png"
            Country.init 99 "Гамбия" "Банжул" "https://upload.wikimedia.org/wikipedia/commons/thumb/7/77/Flag_of_The_Gambia.svg/22px-Flag_of_The_Gambia.svg.png"
            Country.init 100 "Гвинея-Бисау" "Бисау" "https://upload.wikimedia.org/wikipedia/commons/thumb/0/01/Flag_of_Guinea-Bissau.svg/22px-Flag_of_Guinea-Bissau.svg.png"
            Country.init 101 "Республика Конго" "Браззавиль" "https://upload.wikimedia.org/wikipedia/commons/thumb/9/92/Flag_of_the_Republic_of_the_Congo.svg/22px-Flag_of_the_Republic_of_the_Congo.svg.png"
            Country.init 102 "Сейшельские Острова" "Виктория" "https://upload.wikimedia.org/wikipedia/commons/thumb/f/fc/Flag_of_Seychelles.svg/22px-Flag_of_Seychelles.svg.png"
            Country.init 103 "Намибия" "Виндхук" "https://upload.wikimedia.org/wikipedia/commons/thumb/0/00/Flag_of_Namibia.svg/22px-Flag_of_Namibia.svg.png"
            Country.init 104 "Ботсвана" "Габороне" "https://upload.wikimedia.org/wikipedia/commons/thumb/f/fa/Flag_of_Botswana.svg/22px-Flag_of_Botswana.svg.png"
            Country.init 105 "Бурунди" "Гитега" "https://upload.wikimedia.org/wikipedia/commons/thumb/5/50/Flag_of_Burundi.svg/22px-Flag_of_Burundi.svg.png"
            Country.init 106 "Сенегал" "Дакар" "https://upload.wikimedia.org/wikipedia/commons/thumb/f/fd/Flag_of_Senegal.svg/22px-Flag_of_Senegal.svg.png"
            Country.init 107 "Джибути" "Джибути" "https://upload.wikimedia.org/wikipedia/commons/thumb/3/34/Flag_of_Djibouti.svg/22px-Flag_of_Djibouti.svg.png"
            Country.init 108 "Южный Судан" "Джуба" "https://upload.wikimedia.org/wikipedia/commons/thumb/7/7a/Flag_of_South_Sudan.svg/22px-Flag_of_South_Sudan.svg.png"
            Country.init 109 "Танзания" "Додома" "https://upload.wikimedia.org/wikipedia/commons/thumb/3/38/Flag_of_Tanzania.svg/22px-Flag_of_Tanzania.svg.png"
            Country.init 110 "Египет" "Каир" "https://upload.wikimedia.org/wikipedia/commons/thumb/f/fe/Flag_of_Egypt.svg/22px-Flag_of_Egypt.svg.png"
            Country.init 111 "Уганда" "Кампала" "https://upload.wikimedia.org/wikipedia/commons/thumb/4/4e/Flag_of_Uganda.svg/22px-Flag_of_Uganda.svg.png"
            Country.init 112 "Руанда" "Кигали" "https://upload.wikimedia.org/wikipedia/commons/thumb/1/17/Flag_of_Rwanda.svg/22px-Flag_of_Rwanda.svg.png"
            Country.init 113 "ДР Конго" "Киншаса" "https://upload.wikimedia.org/wikipedia/commons/thumb/6/6f/Flag_of_the_Democratic_Republic_of_the_Congo.svg/22px-Flag_of_the_Democratic_Republic_of_the_Congo.svg.png"
            Country.init 114 "Гвинея" "Конакри" "https://upload.wikimedia.org/wikipedia/commons/thumb/e/ed/Flag_of_Guinea.svg/22px-Flag_of_Guinea.svg.png"
            Country.init 115 "Габон" "Либревиль" "https://upload.wikimedia.org/wikipedia/commons/thumb/0/04/Flag_of_Gabon.svg/22px-Flag_of_Gabon.svg.png"
            Country.init 116 "Малави" "Лилонгве" "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d1/Flag_of_Malawi.svg/22px-Flag_of_Malawi.svg.png"
            Country.init 117 "Того" "Ломе" "https://upload.wikimedia.org/wikipedia/commons/thumb/6/68/Flag_of_Togo.svg/22px-Flag_of_Togo.svg.png"
            Country.init 118 "Ангола" "Луанда" "https://upload.wikimedia.org/wikipedia/commons/thumb/9/9d/Flag_of_Angola.svg/22px-Flag_of_Angola.svg.png"
            Country.init 119 "Замбия" "Лусака" "https://upload.wikimedia.org/wikipedia/commons/thumb/0/06/Flag_of_Zambia.svg/22px-Flag_of_Zambia.svg.png"
            Country.init 120 "Экваториальная Гвинея" "Малабо" "https://upload.wikimedia.org/wikipedia/commons/thumb/3/31/Flag_of_Equatorial_Guinea.svg/22px-Flag_of_Equatorial_Guinea.svg.png"
            Country.init 121 "Мозамбик" "Мапуту" "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d0/Flag_of_Mozambique.svg/22px-Flag_of_Mozambique.svg.png"
            Country.init 122 "Лесото" "Масеру" "https://upload.wikimedia.org/wikipedia/commons/thumb/4/4a/Flag_of_Lesotho.svg/22px-Flag_of_Lesotho.svg.png"
            Country.init 123 "Эсватини" "Мбабане" "https://upload.wikimedia.org/wikipedia/commons/thumb/f/fb/Flag_of_Eswatini.svg/22px-Flag_of_Eswatini.svg.png"
            Country.init 124 "Сомали" "Могадишо" "https://upload.wikimedia.org/wikipedia/commons/thumb/a/a0/Flag_of_Somalia.svg/22px-Flag_of_Somalia.svg.png"
            Country.init 125 "Либерия" "Монровия" "https://upload.wikimedia.org/wikipedia/commons/thumb/b/b8/Flag_of_Liberia.svg/22px-Flag_of_Liberia.svg.png"
            Country.init 126 "Коморы" "Морони" "https://upload.wikimedia.org/wikipedia/commons/thumb/9/94/Flag_of_the_Comoros.svg/22px-Flag_of_the_Comoros.svg.png"
            Country.init 127 "Кения" "Найроби" "https://upload.wikimedia.org/wikipedia/commons/thumb/4/49/Flag_of_Kenya.svg/22px-Flag_of_Kenya.svg.png"
            Country.init 128 "Чад" "Нджамена" "https://upload.wikimedia.org/wikipedia/commons/thumb/4/4b/Flag_of_Chad.svg/22px-Flag_of_Chad.svg.png"
            Country.init 129 "Нигер" "Ниамей" "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f4/Flag_of_Niger.svg/22px-Flag_of_Niger.svg.png"
            Country.init 130 "Мавритания" "Нуакшот" "https://upload.wikimedia.org/wikipedia/commons/thumb/4/43/Flag_of_Mauritania.svg/22px-Flag_of_Mauritania.svg.png"
            Country.init 131 "Маврикий" "Порт-Луи" "https://upload.wikimedia.org/wikipedia/commons/thumb/7/77/Flag_of_Mauritius.svg/22px-Flag_of_Mauritius.svg.png"
            Country.init 132 "Бенин" "Порто-Ново" "https://upload.wikimedia.org/wikipedia/commons/thumb/0/0a/Flag_of_Benin.svg/22px-Flag_of_Benin.svg.png"
            Country.init 133 "Кабо-Верде" "Прая" "https://upload.wikimedia.org/wikipedia/commons/thumb/3/38/Flag_of_Cape_Verde.svg/22px-Flag_of_Cape_Verde.svg.png"
            Country.init 134 "ЮАР" "Претория" "https://upload.wikimedia.org/wikipedia/commons/thumb/a/af/Flag_of_South_Africa.svg/22px-Flag_of_South_Africa.svg.png"
            Country.init 135 "Марокко" "Рабат" "https://upload.wikimedia.org/wikipedia/commons/thumb/2/2c/Flag_of_Morocco.svg/22px-Flag_of_Morocco.svg.png"
            Country.init 136 "Сан-Томе и Принсипи" "Сан-Томе" "https://upload.wikimedia.org/wikipedia/commons/thumb/4/4f/Flag_of_Sao_Tome_and_Principe.svg/22px-Flag_of_Sao_Tome_and_Principe.svg.png"
            Country.init 137 "Триполи" "Ливия" "https://upload.wikimedia.org/wikipedia/commons/thumb/0/05/Flag_of_Libya.svg/22px-Flag_of_Libya.svg.png"
            Country.init 138 "Тунис" "Тунис" "https://upload.wikimedia.org/wikipedia/commons/thumb/c/ce/Flag_of_Tunisia.svg/22px-Flag_of_Tunisia.svg.png"
            Country.init 139 "Буркина-Фасо" "Уагадугу" "https://upload.wikimedia.org/wikipedia/commons/thumb/3/31/Flag_of_Burkina_Faso.svg/22px-Flag_of_Burkina_Faso.svg.png"
            Country.init 140 "Сьерра-Леоне" "Фритаун" "https://upload.wikimedia.org/wikipedia/commons/thumb/1/17/Flag_of_Sierra_Leone.svg/22px-Flag_of_Sierra_Leone.svg.png"
            Country.init 141 "Зимбабве" "Хараре" "https://upload.wikimedia.org/wikipedia/commons/thumb/6/6a/Flag_of_Zimbabwe.svg/22px-Flag_of_Zimbabwe.svg.png"
            Country.init 142 "Судан" "Хартум" "https://upload.wikimedia.org/wikipedia/commons/thumb/0/01/Flag_of_Sudan.svg/22px-Flag_of_Sudan.svg.png"
            Country.init 143 "Кот-д’Ивуар" "Ямусукро" "https://upload.wikimedia.org/wikipedia/commons/thumb/f/fe/Flag_of_C%C3%B4te_d%27Ivoire.svg/22px-Flag_of_C%C3%B4te_d%27Ivoire.svg.png"
            Country.init 144 "Камерун" "Яунде" "https://upload.wikimedia.org/wikipedia/commons/thumb/4/4f/Flag_of_Cameroon.svg/22px-Flag_of_Cameroon.svg.png"
            Country.init 145 "Парагвай" "Асунсьон" "https://upload.wikimedia.org/wikipedia/commons/thumb/2/27/Flag_of_Paraguay.svg/22px-Flag_of_Paraguay.svg.png"
            Country.init 146 "Сент-Китс и Невис" "Бастер" "https://upload.wikimedia.org/wikipedia/commons/thumb/f/fe/Flag_of_Saint_Kitts_and_Nevis.svg/22px-Flag_of_Saint_Kitts_and_Nevis.svg.png"
            Country.init 147 "Белиз" "Бельмопан" "https://upload.wikimedia.org/wikipedia/commons/thumb/e/e7/Flag_of_Belize.svg/22px-Flag_of_Belize.svg.png"
            Country.init 148 "Колумбия" "Богота" "https://upload.wikimedia.org/wikipedia/commons/thumb/2/21/Flag_of_Colombia.svg/22px-Flag_of_Colombia.svg.png"
            Country.init 149 "Бразилия" "Бразилиа" "https://upload.wikimedia.org/wikipedia/commons/thumb/0/05/Flag_of_Brazil.svg/22px-Flag_of_Brazil.svg.png"
            Country.init 150 "Барбадос" "Бриджтаун" "https://upload.wikimedia.org/wikipedia/commons/thumb/e/ef/Flag_of_Barbados.svg/22px-Flag_of_Barbados.svg.png"
            Country.init 151 "Аргентина" "Буэнос-Айрес" "https://upload.wikimedia.org/wikipedia/commons/thumb/1/1a/Flag_of_Argentina.svg/22px-Flag_of_Argentina.svg.png"
            Country.init 152 "США" "Вашингтон" "https://upload.wikimedia.org/wikipedia/commons/thumb/a/a4/Flag_of_the_United_States.svg/22px-Flag_of_the_United_States.svg.png"
            Country.init 153 "Куба" "Гавана" "https://upload.wikimedia.org/wikipedia/commons/thumb/b/bd/Flag_of_Cuba.svg/22px-Flag_of_Cuba.svg.png"
            Country.init 154 "Гватемала" "Гватемала" "https://upload.wikimedia.org/wikipedia/commons/thumb/e/ec/Flag_of_Guatemala.svg/22px-Flag_of_Guatemala.svg.png"
            Country.init 155 "Гайана" "Джорджтаун" "https://upload.wikimedia.org/wikipedia/commons/thumb/9/99/Flag_of_Guyana.svg/22px-Flag_of_Guyana.svg.png"
            Country.init 156 "Венесуэла" "Каракас" "https://upload.wikimedia.org/wikipedia/commons/thumb/7/7b/Flag_of_Venezuela_%28state%29.svg/22px-Flag_of_Venezuela_%28state%29.svg.png"
            Country.init 157 "Сент-Люсия" "Кастри" "https://upload.wikimedia.org/wikipedia/commons/thumb/9/9f/Flag_of_Saint_Lucia.svg/22px-Flag_of_Saint_Lucia.svg.png"
            Country.init 158 "Сент-Винсент и Гренадины" "Кингстаун" "https://upload.wikimedia.org/wikipedia/commons/thumb/6/6d/Flag_of_Saint_Vincent_and_the_Grenadines.svg/22px-Flag_of_Saint_Vincent_and_the_Grenadines.svg.png"
            Country.init 159 "Ямайка" "Кингстон" "https://upload.wikimedia.org/wikipedia/commons/thumb/0/0a/Flag_of_Jamaica.svg/22px-Flag_of_Jamaica.svg.png"
            Country.init 160 "Эквадор" "Кито" "https://upload.wikimedia.org/wikipedia/commons/thumb/e/e8/Flag_of_Ecuador.svg/22px-Flag_of_Ecuador.svg.png"
            Country.init 161 "Перу" "Лима" "https://upload.wikimedia.org/wikipedia/commons/thumb/d/df/Flag_of_Peru_%28state%29.svg/22px-Flag_of_Peru_%28state%29.svg.png"
            Country.init 162 "Никарагуа" "Манагуа" "https://upload.wikimedia.org/wikipedia/commons/thumb/1/19/Flag_of_Nicaragua.svg/22px-Flag_of_Nicaragua.svg.png"
            Country.init 163 "Мексика" "Мехико" "https://upload.wikimedia.org/wikipedia/commons/thumb/f/fc/Flag_of_Mexico.svg/22px-Flag_of_Mexico.svg.png"
            Country.init 164 "Уругвай" "Монтевидео" "https://upload.wikimedia.org/wikipedia/commons/thumb/f/fe/Flag_of_Uruguay.svg/22px-Flag_of_Uruguay.svg.png"
            Country.init 165 "Багамские Острова" "Нассау" "https://upload.wikimedia.org/wikipedia/commons/thumb/9/93/Flag_of_the_Bahamas.svg/22px-Flag_of_the_Bahamas.svg.png"
            Country.init 166 "Канада" "Оттава" "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d9/Flag_of_Canada_%28Pantone%29.svg/22px-Flag_of_Canada_%28Pantone%29.svg.png"
            Country.init 167 "Панама" "Панама" "https://upload.wikimedia.org/wikipedia/commons/thumb/a/ab/Flag_of_Panama.svg/22px-Flag_of_Panama.svg.png"
            Country.init 168 "Суринам" "Парамарибо" "https://upload.wikimedia.org/wikipedia/commons/thumb/6/60/Flag_of_Suriname.svg/22px-Flag_of_Suriname.svg.png"
            Country.init 169 "Гаити" "Порт-о-Пренс" "https://upload.wikimedia.org/wikipedia/commons/thumb/5/56/Flag_of_Haiti.svg/22px-Flag_of_Haiti.svg.png"
            Country.init 170 "Тринидад и Тобаго" "Порт-оф-Спейн" "https://upload.wikimedia.org/wikipedia/commons/thumb/6/64/Flag_of_Trinidad_and_Tobago.svg/22px-Flag_of_Trinidad_and_Tobago.svg.png"
            Country.init 171 "Доминика" "Розо" "https://upload.wikimedia.org/wikipedia/commons/thumb/c/c4/Flag_of_Dominica.svg/22px-Flag_of_Dominica.svg.png"
            Country.init 172 "Сальвадор" "Сан-Сальвадор" "https://upload.wikimedia.org/wikipedia/commons/thumb/3/34/Flag_of_El_Salvador.svg/22px-Flag_of_El_Salvador.svg.png"
            Country.init 173 "Коста-Рика" "Сан-Хосе" "https://upload.wikimedia.org/wikipedia/commons/thumb/b/bc/Flag_of_Costa_Rica_%28state%29.svg/22px-Flag_of_Costa_Rica_%28state%29.svg.png"
            Country.init 174 "Доминиканская Республика" "Санто-Доминго" "https://upload.wikimedia.org/wikipedia/commons/thumb/9/9f/Flag_of_the_Dominican_Republic.svg/22px-Flag_of_the_Dominican_Republic.svg.png"
            Country.init 175 "Чили" "Сантьяго" "https://upload.wikimedia.org/wikipedia/commons/thumb/7/78/Flag_of_Chile.svg/22px-Flag_of_Chile.svg.png"
            Country.init 176 "Антигуа и Барбуда" "Сент-Джонс" "https://upload.wikimedia.org/wikipedia/commons/thumb/8/89/Flag_of_Antigua_and_Barbuda.svg/22px-Flag_of_Antigua_and_Barbuda.svg.png"
            Country.init 177 "Гренада" "Сент-Джорджес" "https://upload.wikimedia.org/wikipedia/commons/thumb/b/bc/Flag_of_Grenada.svg/22px-Flag_of_Grenada.svg.png"
            Country.init 178 "Боливия" "Сукре" "https://upload.wikimedia.org/wikipedia/commons/thumb/d/de/Flag_of_Bolivia_%28state%29.svg/22px-Flag_of_Bolivia_%28state%29.svg.png"
            Country.init 179 "Гондурас" "Тегусигальпа" "https://upload.wikimedia.org/wikipedia/commons/thumb/8/82/Flag_of_Honduras.svg/22px-Flag_of_Honduras.svg.png"
            Country.init 180 "Самоа" "Апиа" "https://upload.wikimedia.org/wikipedia/commons/thumb/3/31/Flag_of_Samoa.svg/22px-Flag_of_Samoa.svg.png"
            Country.init 181 "Новая Зеландия" "Веллингтон" "https://upload.wikimedia.org/wikipedia/commons/thumb/3/3e/Flag_of_New_Zealand.svg/22px-Flag_of_New_Zealand.svg.png"
            Country.init 182 "Австралия" "Канберра" "https://upload.wikimedia.org/wikipedia/commons/thumb/b/b9/Flag_of_Australia.svg/22px-Flag_of_Australia.svg.png"
            Country.init 183 "Маршалловы Острова" "Маджуро" "https://upload.wikimedia.org/wikipedia/commons/thumb/2/2e/Flag_of_the_Marshall_Islands.svg/22px-Flag_of_the_Marshall_Islands.svg.png"
            Country.init 184 "Палау" "Нгерулмуд" "https://upload.wikimedia.org/wikipedia/commons/thumb/4/48/Flag_of_Palau.svg/22px-Flag_of_Palau.svg.png"
            Country.init 185 "Тонга" "Нукуалофа" "https://upload.wikimedia.org/wikipedia/commons/thumb/9/9a/Flag_of_Tonga.svg/22px-Flag_of_Tonga.svg.png"
            Country.init 186 "Микронезия" "Паликир" "https://upload.wikimedia.org/wikipedia/commons/thumb/e/e4/Flag_of_the_Federated_States_of_Micronesia.svg/22px-Flag_of_the_Federated_States_of_Micronesia.svg.png"
            Country.init 187 "Вануату" "Порт-Вила" "https://upload.wikimedia.org/wikipedia/commons/thumb/b/bc/Flag_of_Vanuatu.svg/22px-Flag_of_Vanuatu.svg.png"
            Country.init 188 "Папуа — Новая Гвинея" "Порт-Морсби" "https://upload.wikimedia.org/wikipedia/commons/thumb/e/e3/Flag_of_Papua_New_Guinea.svg/22px-Flag_of_Papua_New_Guinea.svg.png"
            Country.init 189 "Фиджи" "Сува" "https://upload.wikimedia.org/wikipedia/commons/thumb/b/ba/Flag_of_Fiji.svg/22px-Flag_of_Fiji.svg.png"
            Country.init 190 "Тувалу" "Фунафути" "https://upload.wikimedia.org/wikipedia/commons/thumb/3/38/Flag_of_Tuvalu.svg/22px-Flag_of_Tuvalu.svg.png"
            Country.init 191 "Соломоновы Острова" "Хониара" "https://upload.wikimedia.org/wikipedia/commons/thumb/7/74/Flag_of_the_Solomon_Islands.svg/22px-Flag_of_the_Solomon_Islands.svg.png"
            Country.init 192 "Кирибати" "Южная Тарава (Баирики)" "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d3/Flag_of_Kiribati.svg/22px-Flag_of_Kiribati.svg.png"
            Country.init 193 "Науру" "Официальной столицы нет" "https://upload.wikimedia.org/wikipedia/commons/thumb/3/30/Flag_of_Nauru.svg/22px-Flag_of_Nauru.svg.png"
        |]
        |> Array.map (fun country -> country.Id, country)
        |> Map.ofArray

    let difficultInit id name countries =
        countries
        |> Array.map (fun countryName ->
            let res =
                countriesById
                |> Seq.tryFind (fun (KeyValue(_, country)) ->
                    country.Name = countryName
                )
            match res  with
            | Some x -> x.Key
            | None ->
                failwithf "not found %s" countryName
        )
        |> Difficult.init id name

    let countriesByDifficult: CountriesByDifficult =
        [|
            difficultInit 0 "легкий"
                [|
                    "Нидерланды"
                    "Андорра"
                    "Греция"
                    "Сербия"
                    "Германия"
                    "Швейцария"
                    "Словакия"
                    "Бельгия"
                    "Венгрия"
                    "Румыния"
                    "Лихтенштейн"
                    "Мальта"
                    "Польша"
                    "Ватикан"
                    "Австрия"
                    "Литва"
                    "Ирландия"
                    "Хорватия"
                    "Украина"
                    "Молдавия"
                    "Дания"
                    "Португалия"
                    "Великобритания"
                    "Словения"
                    "Люксембург"
                    "Испания"
                    "Белоруссия"
                    "Монако"
                    "Россия"
                    "Норвегия"
                    "Франция"
                    "Черногория"
                    "Чехия"
                    "Исландия"
                    "Латвия"
                    "Италия"
                    "Сан-Марино"
                    "Босния и Герцеговина"
                    "Северная Македония"
                    "Болгария"
                    "Швеция"
                    "Эстония"
                    "Албания"
                    "Финляндия"
                    "ОАЭ"
                    "Иордания"
                    "Турция"
                    "Туркменистан"
                    "Ирак"
                    "Азербайджан"
                    "Таиланд"
                    "Бруней"
                    "Ливан"
                    "Киргизия"
                    "Лаос"
                    "Бангладеш"
                    "Сирия"
                    "Индия"
                    "Индонезия"
                    "Восточный Тимор"
                    "Катар"
                    "Таджикистан"
                    "Армения"
                    "Израиль"
                    "Пакистан"
                |]
            difficultInit 1 "средний"
                [|
                    "Афганистан"
                    "Непал"
                    "Малайзия"
                    "Мальдивы"
                    "Бахрейн"
                    "Филиппины"
                    "Оман"
                    "Мьянма"
                    "Кипр"
                    "Казахстан"
                    "Китай"
                    "Камбоджа"
                    "КНДР"
                    "Йемен"
                    "Республика Корея"
                    "Сингапур"
                    "Узбекистан"
                    "Грузия"
                    "Иран"
                    "Япония"
                    "Бутан"
                    "Монголия"
                    "Вьетнам"
                    "Шри-Ланка"
                    "Кувейт"
                    "Саудовская Аравия"
                    "Нигерия"
                    "Эфиопия"
                    "Гана"
                    "Алжир"
                    "Мадагаскар"
                    "Эритрея"
                    "Мали"
                    "ЦАР"
                    "Гамбия"
                    "Гвинея-Бисау"
                    "Республика Конго"
                    "Сейшельские Острова"
                    "Намибия"
                    "Ботсвана"
                    "Бурунди"
                    "Сенегал"
                    "Джибути"
                    "Южный Судан"
                    "Танзания"
                    "Египет"
                    "Уганда"
                    "Руанда"
                    "ДР Конго"
                    "Гвинея"
                    "Габон"
                    "Малави"
                    "Того"
                    "Ангола"
                    "Замбия"
                    "Экваториальная Гвинея"
                    "Мозамбик"
                    "Лесото"
                    "Эсватини"
                    "Сомали"
                    "Либерия"
                    "Коморы"
                    "Кения"
                    "Чад"
                    "Нигер"
                |]
            difficultInit 2 "тяжелый"
                [|
                    "Мавритания"
                    "Маврикий"
                    "Бенин"
                    "Кабо-Верде"
                    "ЮАР"
                    "Марокко"
                    "Сан-Томе и Принсипи"
                    "Триполи"
                    "Тунис"
                    "Буркина-Фасо"
                    "Сьерра-Леоне"
                    "Зимбабве"
                    "Судан"
                    "Кот-д’Ивуар"
                    "Камерун"
                    "Парагвай"
                    "Сент-Китс и Невис"
                    "Белиз"
                    "Колумбия"
                    "Бразилия"
                    "Барбадос"
                    "Аргентина"
                    "США"
                    "Куба"
                    "Гватемала"
                    "Гайана"
                    "Венесуэла"
                    "Сент-Люсия"
                    "Сент-Винсент и Гренадины"
                    "Ямайка"
                    "Эквадор"
                    "Перу"
                    "Никарагуа"
                    "Мексика"
                    "Уругвай"
                    "Багамские Острова"
                    "Канада"
                    "Панама"
                    "Суринам"
                    "Гаити"
                    "Тринидад и Тобаго"
                    "Доминика"
                    "Сальвадор"
                    "Коста-Рика"
                    "Доминиканская Республика"
                    "Чили"
                    "Антигуа и Барбуда"
                    "Гренада"
                    "Боливия"
                    "Гондурас"
                    "Самоа"
                    "Новая Зеландия"
                    "Австралия"
                    "Маршалловы Острова"
                    "Палау"
                    "Тонга"
                    "Микронезия"
                    "Вануату"
                    "Папуа — Новая Гвинея"
                    "Фиджи"
                    "Тувалу"
                    "Соломоновы Острова"
                    "Кирибати"
                    "Науру"
                |]
        |]
        |> Array.map (fun x -> x.Id, x)
        |> Map.ofArray

    let countries: Countries =
        countriesById
        |> Seq.map (fun (KeyValue(id, _)) -> id)
        |> Array.ofSeq

    countriesByDifficult, countriesById, countries
