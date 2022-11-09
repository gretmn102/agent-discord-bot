module SimpleQuiz.Model

type QuestionId = string
type QuizId = string

type Question = {
    Id: QuestionId
    Description: string
    Correct: string
    Others: string []
}

type Quiz =
    {
        Id: QuizId
        Name: string
        Questions: Question []
    }

let createQuiz quizId (questions: Question []) =
    {
        Id = quizId
        Name = quizId
        Questions =
            questions
            |> Array.mapi (fun i x ->
                { x with Id = string i }
            )
    }

let quizes =
    let capitals =
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
        |]

    let flags =
        [|
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
    [|
        createQuiz "capitals" capitals
        createQuiz "flags" flags
    |]
    |> Array.map (fun q -> q.Id, q)
    |> Map.ofArray

let quizByQuestionId =
    quizes
    |> Map.map (fun k quiz ->
        let questions =
            quiz.Questions
            |> Array.map (fun x -> x.Id, x)
            |> Map.ofArray

        questions
    )
