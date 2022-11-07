module SimpleQuiz.Model

type QuestionId = string

type Question = {
    Id: QuestionId
    Description: string
    Correct: string
    Others: string []
}

type Quiz = Question []

let quiz: Quiz =
    [|
        {
            Id = ""
            Description = "Какая столица страны \"Украина\"?"
            Correct = "Киев"
            Others = [|
                "София"
                "Рим"
                "Бангкок"
                "Ташкент"
                "Асунсьон"
            |]
        }
        {
            Id = ""
            Description = "Какая столица страны \"Мексика\"?"
            Correct = "Мехико"
            Others = [|
                "Бразилия"
                "Гавана"
                "Тегеран"
                "Рига"
                "Братислава"
            |]
        }
        {
            Id = ""
            Description = "Какая столица страны \"Филиппины\"?"
            Correct = "Манила"
            Others = [|
                "Копенгаген"
                "Ташкент"
                "Хараре"
                "Прага"
                "Рейкьявик"
                "Нью-Дели"
                "Астана"
                "Кишинёв"
                "Лиссабон"
                "Минск"
                "Гавана"
            |]
        }
        {
            Id = ""
            Description = "Какой флаг имеет страна \"Южная Корея\"?"
            Correct = ":flag_kr:"
            Others = [|
                ":flag_af:"
                ":flag_ar:"
                ":flag_pk:"
                ":flag_mn:"
                ":flag_pt:"
            |]
        }
        {
            Id = ""
            Description = "Какой флаг имеет страна \"Саудовская Аравия\"?"
            Correct = ":flag_sa:"
            Others = [|
                ":flag_ug:"
                ":flag_sy:"
                ":flag_ee:"
                ":flag_es:"
                ":flag_se:"
            |]
        }
        {
            Id = ""
            Description = "Какой флаг имеет страна \"Индия\"?"
            Correct = ":flag_in:"
            Others = [|
                ":flag_iq:"
                ":flag_de:"
                ":flag_md:"
                ":flag_au:"
                ":flag_ee:"
                ":flag_se:"
                ":flag_ir:"
                ":flag_np:"
                ":flag_kp:"
                ":flag_es:"
            |]
        }
    |]
    |> Array.mapi (fun i x -> { x with Id = string i })

let quizByQuestionId =
    quiz
    |> Array.map (fun x -> x.Id, x)
    |> Map.ofArray
