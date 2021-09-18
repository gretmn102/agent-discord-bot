module EmojiFont
open FsharpMyExtension
open FsharpMyExtension.Either
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats
open SixLabors.ImageSharp.Processing
open SixLabors.ImageSharp.Drawing.Processing

module Image =
    let toArray (image: Image<Rgba32>) =
        Array.init image.Height (fun y ->
            let pixelRowSpan = image.GetPixelRowSpan(y)
            pixelRowSpan.ToArray()
        )
    let iter fn (image: Image<Rgba32>) =
        for y = 0 to image.Height - 1 do
            let pixelRowSpan = image.GetPixelRowSpan(y)
            for x = 0 to image.Width - 1 do
                fn x y pixelRowSpan.[x]

    let mapToArray fn (image: Image<Rgba32>) =
        let xss = Array.init image.Height (fun _ -> Array.zeroCreate image.Width)
        for y = 0 to image.Height - 1 do
            let pixelRowSpan = image.GetPixelRowSpan(y)
            let xs = xss.[y]
            for x = 0 to image.Width - 1 do
                xs.[x] <- fn x y pixelRowSpan.[x]
        xss

module Array2D =
    // TODO: {| LeftTrim: bool; RightTrim: bool; TopTrim: bool; BottomTrim: bool |}
    let trimLeftRight isEmpty (xss: 'a [] []) =
        let allEmptyColumn x =
            xss
            |> Array.forall (fun xs ->
                isEmpty xs.[x]
            )

        let width = xss.[0].Length

        let leftMost =
            let rec apply x =
                if x < width && allEmptyColumn x then
                    apply (x + 1)
                else
                    x
            apply 0

        let rightMost =
            let rec apply x =
                if x >= 0 && allEmptyColumn x then
                    apply (x - 1)
                else
                    x + 1
            apply (width - 1)

        if leftMost > rightMost then
            failwithf "leftMost > rightMost = %d > %d" leftMost rightMost
        else
            xss
            |> Array.map (fun xs ->
                let diff = rightMost - leftMost
                Array.init diff (fun i ->
                    xs.[leftMost + i]
                )
            )

    open Fuchu
    [<Tests>]
    let trimLeftRightTests =
        testList "trimLeftRightTests" [
            testCase "testCase1" (fun _ ->
                let input =
                    [|
                        [| 1 |]
                        [| 2 |]
                    |]
                let exp =
                    [|
                        [| 1 |]
                        [| 2 |]
                    |]
                let act = trimLeftRight ((=) 0) input
                Assert.Equal("", exp, act)
            )
            testCase "testCase1" (fun _ ->
                let input =
                    [|
                        [| 0; 1; 0 |]
                        [| 0; 2; 0 |]
                    |]
                let exp =
                    [|
                        [| 1 |]
                        [| 2 |]
                    |]
                let act = trimLeftRight ((=) 0) input
                Assert.Equal("", exp, act)
            )
            testCase "testCase1" (fun _ ->
                let input =
                    [|
                        [| 0; 1; 2; 0 |]
                        [| 0; 3; 4; 0 |]
                    |]
                let exp =
                    [|
                        [| 1; 2 |]
                        [| 3; 4 |]
                    |]
                let act = trimLeftRight ((=) 0) input
                Assert.Equal("", exp, act)
            )
        ]

type Mask = bool [] []

let charSheetRus, charImageSheetRus, (charWidthRus, charHeightRus) =
    let fontPath = @"rus_lowercase_sheet.png"
    use font = Image.Load(fontPath)

    let width, height = font.Width, font.Height
    let widthCount, heightCount = [|'а'..'я'|].Length, 1
    let charWidth, charHeight = width / widthCount, height / heightCount

    let graphicsOptions = GraphicsOptions()
    let charImageSheet: Mask array array =
        Array.init heightCount (fun y ->
            Array.init widthCount (fun x ->
                use charImage = new Image<Rgba32>(charWidth, charHeight)

                charImage.Mutate(fun ctx ->
                    use src =
                        font.Clone(fun ctx ->
                            let r = Rectangle(x * charWidth, y * charHeight, charWidth, charHeight)

                            ctx.Crop r
                            |> ignore
                        )
                    ctx.DrawImage(src, graphicsOptions)
                    |> ignore
                )

                charImage
                |> Image.mapToArray(fun x y color ->
                    color.A <> 0uy
                )
                |> Array2D.trimLeftRight not
            )
        )

    let charSheet =
        let xss =
            [|
                [|'а'..'я'|]
            |]
        xss
        |> Array.mapi (fun y ->
            Array.mapi (fun x c -> c, (x, y))
        )
        |> Array.concat
        |> Map.ofArray
    charSheet, charImageSheet, (charWidth, charHeight)

let getLength pixelSize shift (str:string) =
    str
    |> Seq.fold
        (fun acc c ->
            match Map.tryFind c charSheetRus with
            | Some (x, y) ->
                let mask = charImageSheetRus.[y].[x]

                let charWidth = mask.[0].Length
                acc + charWidth * pixelSize + shift
            | None ->
                let charWidth = 6 // TODO: unknown char
                acc + charWidth * pixelSize + shift
        )
        0
    |> fun x -> x - shift

let drawText (srcImg: Either<string,byte array>) (str: string) (outputStream: System.IO.MemoryStream) =
    use srcImg =
        match srcImg with
        | Right bytes ->
            Image.Load bytes
        | Left _ ->
            let img = new Image<Rgba32>(16, 16)
            img.Mutate (fun ctx ->
                ctx.Fill(Color(Rgba32(255uy, 255uy, 255uy)))
                |> ignore
            )
            img

    let shift = 1 * srcImg.Width

    use image = new Image<Rgba32>(getLength srcImg.Width shift str, charHeightRus * srcImg.Height)
    let graphicsOptions = GraphicsOptions()
    image.Mutate (fun ctx ->
        str
        |> Seq.fold
            (fun acc c ->
                match Map.tryFind c charSheetRus with
                | Some (x, y) ->
                    let mask = charImageSheetRus.[y].[x]

                    mask
                    |> Array.iteri (fun y ->
                        Array.iteri (fun x m ->
                            if m then
                                ctx.DrawImage(srcImg, Point(x * srcImg.Width + acc, y * srcImg.Height), graphicsOptions)
                                |> ignore
                        )
                    )

                    let charWidth = mask.[0].Length
                    acc + charWidth * srcImg.Width + shift
                | None ->
                    let charWidth = 6 // TODO: unknown char
                    acc + charWidth * srcImg.Width + shift
            )
            0
        |> ignore
    )

    image.Save(outputStream, Formats.Png.PngFormat.Instance)
