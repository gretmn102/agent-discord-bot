module NumberToWords.Core
open FsharpMyExtension

let twoToTen =
    [|
        "два"
        "три"
        "четыре"
        "пять"
        "шесть"
        "семь"
        "восемь"
        "девять"
    |]
let lessTen =
    [|
        yield "один"
        yield! twoToTen
    |]

let lessTwenty =
    [|
        "десять"
        "одиннадцать"
        "двенадцать"
        "тринадцать"
        "четырнадцать"
        "пятнадцать"
        "шестнадцать"
        "семнадцать"
        "восемнадцать"
        "девятнадцать"
    |]

let twentyToHundred =
    [|
        "двадцать"
        "тридцать"

        "сорок"

        "пятьдесят"
        "шестьдесят"
        "семьдесят"
        "восемьдесят"

        "девяносто"
    |]

let hundredth =
    [|
        "сто"
        "двести"
        "триста"
        "четыреста"
        "пятьсот"
        "шестьсот"
        "семьсот"
        "восемьсот"
        "девятьсот"
    |]

let thousandth =
    [|
        "одна тысяча"
        "две тысячи"
        "три тысячи"
        "четыре тысячи"
        "пять тысяч"
        "шесть тысяч"
        "семь тысяч"
        "восемь тысяч"
        "девять тысяч"
    |]

let template str =
    [|
        sprintf "один %s" str
        sprintf "два %sа" str
        sprintf "три %sа" str
        sprintf "четыре %sа" str
        sprintf "пять %sов" str
        sprintf "шесть %sов" str
        sprintf "семь %sов" str
        sprintf "восемь %sов" str
        sprintf "девять %sов" str
    |]

let bigNumbers =
    let xs =
        [
            "миллион"
            "миллиард"
            "триллион"
            "квадриллион"
            "квинтиллион"
            "секстиллион"
            "септиллион"
            "октиллион"
            "нониллион"
            "дециллион"
            "ундециллион"
            "дуодециллион"
            "тредециллион"
            "кваттуордециллион"
            "квиндециллион"
            "сексдециллион"
            "септдециллион"
            "октодециллион"
            "новемдециллион"
            "вигинтиллион"
            "унвигинтиллион"
            "дуовигинтиллион"
            "тревигинтиллион"
            "кваттуорвигинтиллион"
            "квинвигинтиллион"
            "сексвигинтиллион"
            "септенвигинтиллион"
            "октовигинтиллион"
            "новемвигинтиллион"
            "тригинтиллион"
            "унтригинтиллион"
            "дуотригинтиллион"
            "третригинтиллион"
            "кваттуортригинтиллион"
            "квинтригинтиллион"
            "секстригинтиллион"
            "септентригинтиллион"
            "октотригинтиллион"
            "новемтригинтиллион"
            "квадрагинтиллион"

        ]
    let ys = Seq.unfold (fun oldAcc -> let newAcc = oldAcc * 1000I in Some((newAcc, oldAcc), newAcc)) 1000000I
    Seq.zip ys (List.map (fun x -> sprintf "%sов" x, template x) xs)
    |> Seq.map (fun ((x, y), (w, z)) -> x, (y, w, z))
    |> List.ofSeq


let toNumName n =
    let rec f n =
        let gen k name (xs:_ []) =
            let rec f' n =
                if n < 10I then
                    xs.[int <| n - 1I]
                elif n < 20I then
                    lessTwenty.[int <| n % 10I]
                    |> flip (sprintf "%s %s") name
                elif n < 100I then
                    let mod' = n % 10I
                    let first = twentyToHundred.[int <| n / 10I - 2I]
                    if mod' = 0I then
                        first
                        |> flip (sprintf "%s %s") name
                    else
                        sprintf "%s %s" first (f' mod')
                else
                    let mod' = n % 100I
                    let first = hundredth.[int <| n / 100I - 1I]
                    if mod' = 0I then
                        first
                        |> flip (sprintf "%s %s") name
                    else
                        sprintf "%s %s" first (f' mod')
            let mod' = n % k
            let first = f' (n / k)
            if mod' = 0I then first
            else
                sprintf "%s %s" first (f mod')
        if n < 10I then
            lessTen.[int(n - 1I)]
        elif n < 20I then
            lessTwenty.[int(n % 10I)]
        elif n < 100I then
            let mod' = n % 10I
            if mod' = 0I then ""
            else sprintf " %s" (f mod')
            |> (+) twentyToHundred.[int <| n / 10I - 2I]
        elif n < 1000I then
            let mod' = n % 100I
            if mod' = 0I then ""
            else sprintf " %s" (f mod')
            |> (+) hundredth.[int <| n / 100I - 1I]
        elif n < 1000000I then
            gen 1000I "тысяч" thousandth
        // elif n < 1000000000I then
        //     gen 1000000I "миллионов" millionth
        // elif n < 1000000000000I then
        //     gen 1000000000I "миллиардов" milliard
        // elif n < 1000000000000000I then
        //     gen 1000000000000I "триллионов" threellion
        // else
        //     failwith "greater than or equal 1 000 000 000 not implement"
        else
            match List.tryFind (fun (x, _) -> n < x) bigNumbers with
            | Some (_, (x, name, names)) ->
                gen x name names
            | None ->
                failwithf "greater than or equal %A not implement" (List.last bigNumbers)

    if n < 0I then sprintf "минус %s" (f (abs n))
    elif n = 0I then "ноль"
    else f n
