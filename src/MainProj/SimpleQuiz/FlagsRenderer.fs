module SimpleQuiz.FlagsRenderer
open FsharpMyExtension
open FsharpMyExtension.Either
open System.Drawing

let getTextSize =
    let img = new Bitmap(1, 1)
    fun font str ->
        use g = Graphics.FromImage(img)

        g.SmoothingMode <- Drawing2D.SmoothingMode.HighQuality
        g.TextRenderingHint <- Text.TextRenderingHint.AntiAlias
        g.InterpolationMode <- Drawing2D.InterpolationMode.HighQualityBicubic
        g.PixelOffsetMode <- Drawing2D.PixelOffsetMode.HighQuality

        g.MeasureString(str, font)

let font =
    let collection = new Text.PrivateFontCollection()
    collection.AddFontFile "Fonts/DejaVuSansCondensed.ttf"
    new Font(collection.Families.[0], 14.f, FontStyle.Regular)

let drawText text font textColor (x: float32, y: float32) (canvas: Image) =
    use g = Graphics.FromImage(canvas)

    g.SmoothingMode <- Drawing2D.SmoothingMode.HighQuality
    g.TextRenderingHint <- Text.TextRenderingHint.AntiAlias
    g.InterpolationMode <- Drawing2D.InterpolationMode.HighQualityBicubic
    g.PixelOffsetMode <- Drawing2D.PixelOffsetMode.HighQuality

    use txtBrush = new SolidBrush(textColor)
    g.DrawString(text, font, txtBrush, x, y)

let textColor = Color.Black

/// ```
/// |¯¯¯¯¯¯¯¯¯¯|
/// |   num    |
/// |          |
/// | |¯¯¯¯¯¯| |
/// | | flag | |
/// | |______| |
/// |__________|
/// ```
let drawNumberAndFlag (number: int) (flagImage: Image) (canvas: Image) =
    let numberStr = string number
    let textSize = getTextSize font numberStr

    let flagWidthHalf = flagImage.Width / 2
    let canvasWidthHalf = canvas.Width / 2
    let canvasHeightHalf = canvas.Height / 2

    let betweenLength = 0

    let textWidthHalf = int (textSize.Width / 2.f)
    let textHeightHalf = int (textSize.Height / 2.f)
    let textHeight = int textSize.Height

    let textX = canvasWidthHalf - textWidthHalf
    let flagX = canvasWidthHalf - flagWidthHalf
    let textAndFlagHeight = textHeight + betweenLength + flagImage.Height
    let textAndFlagHeightHalf = textAndFlagHeight / 2
    let textAndFlagCenterY = canvasHeightHalf - textAndFlagHeightHalf
    let textY = textAndFlagCenterY
    let flagY = textAndFlagCenterY + textHeight + betweenLength

    drawText numberStr font textColor (float32 textX, float32 textY) canvas

    // draw flag
    do
        use g = Graphics.FromImage(canvas)
        let flagRect = Rectangle(0, 0, flagImage.Width, flagImage.Height)
        let dstRect = Rectangle(flagX, flagY, flagImage.Width, flagImage.Height)
        g.DrawImage(flagImage, dstRect, flagRect, GraphicsUnit.Pixel)

        // draws the border around the flag
        let p = new Pen(Color.Black)
        let penWidth = int p.Width
        g.DrawRectangle(p, Rectangle(flagX - penWidth, flagY - penWidth, flagImage.Width + penWidth, flagImage.Height + penWidth))

let drawFlagsOnGrid (columnsCount, rowsCount) (numberAndFlags: (int * Image) []) =
    let (cellWidth, cellHeight) = 32, 46

    let numberAndFlags =
        numberAndFlags
        |> Seq.map (fun (number, flag) ->
            let canvas = new Bitmap(cellWidth, cellHeight)

            do
                use g = Graphics.FromImage canvas
                g.Clear(Color.White)

            drawNumberAndFlag number flag canvas

            canvas, Rectangle(0, 0, cellWidth, cellHeight)
        )
        |> List.ofSeq

    let res =
        numberAndFlags
        |> Grid.drawImagesOnGrids
            Color.White
            1
            false
            (cellWidth, cellHeight)
            (columnsCount, rowsCount)
        |> Seq.head

    numberAndFlags
    |> List.iter (fun (img, _) -> img.Dispose())

    res

let downloadAndDrawFlags (urls: string seq) =
    let flags =
        urls
        |> Seq.map (fun url ->
            let headers =
                [
                    // The remote server returned an error: (403) Forbidden. Please comply with the User-Agent policy: https://meta.wikimedia.org/wiki/User-Agent_policy.
                    "User-Agent", "Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:98.0) Gecko/20100101 Firefox/98.0"
                ]
            WebClientDownloader.getData headers url
            |> Either.map (fun x ->
                use m = new System.IO.MemoryStream(x)
                let flag = new Bitmap(m)
                flag
            )
        )
        |> List.ofSeq
        |> List.seqEither

    // WebClient does not support concurrent I/O operations, therefore, do not even think to paralle them

    flags
    |> Either.map (fun xs ->
        let xs =
            xs
            |> List.mapi (fun i bmp -> i + 1, bmp :> Image)
            |> Array.ofList
        drawFlagsOnGrid (4, 2) xs
    )
