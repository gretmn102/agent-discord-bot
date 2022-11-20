module SimpleQuiz.FlagsRenderer
open FsharpMyExtension
open FsharpMyExtension.Either
open FsharpMyExtension.ResultExt
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

let downloads urls webCacher =
    webCacher
    |> WebCacher.gets
        id
        (fun bytes ->
            match bytes.Content with
            | WebDownloader.Binary bytes ->
                use m = new System.IO.MemoryStream(bytes)
                let flag = new Bitmap(m)
                flag
            | x ->
                failwithf "expected Binary but Text\n%A" x
        )
        urls

let downloadAndDrawFlags (webCacher: WebCacher<Bitmap>) (urls: string seq) =
    let flags, webCacher =
        downloads urls webCacher

    let bmp =
        flags
        |> Seq.mapi (fun i (_, res) ->
            let bmp =
                match res with
                | Ok res -> res.Data

                | Error errMsg ->
                    printfn "%A" errMsg
                    new Bitmap(1, 1)

            i + 1, bmp :> Image
        )
        |> Array.ofSeq
        |> drawFlagsOnGrid (4, 2)

    bmp, webCacher
