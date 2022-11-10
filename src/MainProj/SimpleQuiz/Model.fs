module SimpleQuiz.Model
open FsharpMyExtension
open MongoDB.Driver
open MongoDB.Bson

open Types

type QuestionId = string
type QuizId = string

type Question = {
    Id: QuestionId
    Description: string
    Correct: string
    Others: string []
}

type Quiz<'Questions> =
    {
        Id: QuizId
        Name: string
        Questions: 'Questions
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Quiz =
    let create id name questions =
        {
            Id = id
            Name = name
            Questions = questions
        }

type QuizArray = Quiz<Question []>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module QuizArray =
    let create quizId (questions: Question []): QuizArray =
        {
            Id = quizId
            Name = quizId
            Questions =
                questions
                |> Array.mapi (fun i x ->
                    { x with Id = string i }
                )
        }

type QuizMap = Quiz<Map<QuestionId, Question>>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module QuizMap =
    let ofQuizArray (quizArr: QuizArray): QuizMap =
        {
            Id = quizArr.Id
            Name = quizArr.Name
            Questions =
                quizArr.Questions
                |> Array.map (fun x -> x.Id, x)
                |> Map.ofArray
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
        QuizArray.create "capitals" capitals
        QuizArray.create "flags" flags
    |]
    |> Array.map (fun q -> q.Id, q)
    |> Map.ofArray

let quizByQuestionId =
    quizes
    |> Map.map (fun k quiz ->
        QuizMap.ofQuizArray quiz
    )

module Players =
    type MainData =
        {
            Wins: int
            Loses: int
        }
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module MainData =
        let empty =
            {
                Wins = 0
                Loses = 0
            }

        let serialize (data: MainData) =
            data |> Json.ser

        let deserialize json =
            try
                let res: MainData = Json.des json
                Ok res
            with e ->
                Error e.Message

    type Id = { UserId: UserId; QuizId: QuizId }
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Id =
        let create userId quizId =
            { UserId = userId; QuizId = quizId }

    type Version =
        | V0 = 0

    type Data<'MainData> =
        {
            Id: Id
            Version: Version
            Data: 'MainData
        }
        static member Init(id: Id, data: 'MainData) =
            {
                Id = id
                Version = Version.V0
                Data = data
            }

    type Collection = IMongoCollection<BsonDocument>

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Collection =
        let replace (newData: Data<MainData>) (collection: Collection) =
            let doc = newData.ToBsonDocument()

            let el = BsonElement("_id", newData.Id.ToBsonDocument())
            let i = new BsonDocument(el)

            let filter = FilterDefinition.op_Implicit(i)

            collection.ReplaceOne(filter, doc)
            |> ignore

        let insert (id: Id) setAdditionParams (collection: Collection) =
            let x =
                Data.Init(id, setAdditionParams MainData.empty)

            let d = x.ToBsonDocument()
            collection.InsertOne(d)

            x

    type GuildData =
        {
            Cache: Map<QuizId, Map<UserId, Data<MainData>>>
            Collection: Collection
        }

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module GuildData =
        let set (id: Id) setAdditionParams (guildData: GuildData) =
            let cache = guildData.Cache

            {
                guildData with
                    Cache =
                        let newPlayersWithCurrent () =
                            let player =
                                guildData.Collection
                                |> Collection.insert id setAdditionParams

                            Map.add id.UserId player Map.empty

                        let players =
                            match Map.tryFind id.QuizId cache with
                            | Some players ->
                                match Map.tryFind id.UserId players with
                                | Some player ->
                                    let data =
                                        { player with
                                            Data = setAdditionParams player.Data
                                        }

                                    Collection.replace data guildData.Collection

                                    Map.add id.UserId data players
                                | None ->
                                    newPlayersWithCurrent ()

                            | None ->
                                newPlayersWithCurrent ()

                        Map.add id.QuizId players cache
            }

        let collectionName = "simpleQuizPlayers"

        let init (db: IMongoDatabase): GuildData =
            let collection = db.GetCollection<BsonDocument>(collectionName)

            if IMongoCollection.isEmpty collection then
                {
                    Cache = Map.empty
                    Collection = collection
                }
            else
                {
                    Cache =
                        collection.Find(fun x -> true).ToEnumerable()
                        |> Seq.fold
                            (fun st x ->
                                let ver =
                                    match x.["Version"] with
                                    | null -> failwithf "`Version` but\n%A" x
                                    | x ->
                                        if x.IsInt32 then
                                            enum<Version> x.AsInt32
                                        else
                                            failwithf "Version not int32 but %A" x
                                let player =
                                    match ver with
                                    | Version.V0 ->
                                        Serialization.BsonSerializer.Deserialize<Data<MainData>>(x)
                                    | x ->
                                        failwithf "Version = %A not implemented" x

                                st
                                |> Map.addOrModWith
                                    player.Id.QuizId
                                    (fun () ->
                                        Map.add player.Id.UserId player Map.empty
                                    )
                                    (fun players ->
                                        players
                                        |> Map.add player.Id.UserId player
                                    )
                            )
                            Map.empty
                    Collection = collection
                }

        let drop (db: IMongoDatabase) (guildWelcomeSetting: GuildData) =
            db.DropCollection collectionName

            { guildWelcomeSetting with
                Cache = Map.empty
            }

        let tryFindPlayer (id: Id) (guildWelcomeSetting: GuildData) =
            Map.tryFind id.QuizId guildWelcomeSetting.Cache
            |> Option.bind (fun players ->
                Map.tryFind id.UserId players
            )

        let tryFindQuizPlayers quizId (guildWelcomeSetting: GuildData) =
            Map.tryFind quizId guildWelcomeSetting.Cache
