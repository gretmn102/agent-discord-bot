module FlagsRendererTests
open Fuchu
open FsharpMyExtension
open System.Drawing

open SimpleQuiz

[<Tests>]
let drawNumberAndFlagTests =
    testList "drawNumberAndFlagTests" [
        testCase "base" <| fun () ->
            use flag = new Bitmap("SimpleQuiz/mocks/22px-Flag_of_the_Netherlands.svg.png")

            let flagWithNumberMockPath = "SimpleQuiz/mocks/flagWithNumberMock.png"

            let act =
                use canvas = new Bitmap(32, 46)
                use g = Graphics.FromImage canvas
                g.Clear(Color.White)
                FlagsRenderer.drawNumberAndFlag 42 flag canvas

                // canvas.Save(flagWithNumberMockPath)

                Bitmap.toArray canvas

            let exp =
                use flagWithNumberMock = new Bitmap(flagWithNumberMockPath)
                Bitmap.toArray flagWithNumberMock

            Assert.Equal("", exp, act)
    ]

[<Tests>]
let drawFlagsOnGridTests =
    testList "drawFlagsOnGridTests" [
        testCase "base" <| fun () ->
            use flag = new Bitmap("SimpleQuiz/mocks/22px-Flag_of_the_Netherlands.svg.png")

            let flagsMock = "SimpleQuiz/mocks/flagsMock.png"

            let act =
                let res =
                    Array.init 8 (fun i ->
                        i, flag :> Image
                    )
                    |> FlagsRenderer.drawFlagsOnGrid (4, 2)

                // res.Save(flagsMock)

                Bitmap.toArray res

            let exp =
                use mockImage = new Bitmap(flagsMock)
                Bitmap.toArray mockImage

            Assert.Equal("", exp, act)
    ]
