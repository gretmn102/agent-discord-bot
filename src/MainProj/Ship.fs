module Ship
open FsharpMyExtension
open FsharpMyExtension.Either

module WebClientDownloader =
    open FsharpMyExtension.WebClientDownloader

    let getData (headers:(string * string) list) (url:string) =
        let hd = webClient.Headers

        headers |> List.iter hd.Set
        try
            webClient.DownloadData url
            |> Right
        with
            | x -> Left x.Message

open System.Drawing

// System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

/// blue frame + heart + blue frame
let (width, height), templateBmp =
    let path = @"heart.png"
    // let path = @"heart100.png"
    use heart = new Bitmap(path)

    let width, height = 133, 133 // heart.Width, heart.Height
    let dstBmp = new Bitmap(width * 3, height)

    use g = Graphics.FromImage dstBmp
    g.DrawImage(heart, width, 0, width, height)
    // g.DrawImageUnscaled(heart, 0, 0)

    do
        let c = Color.FromArgb 0xff2980b9
        use p = new SolidBrush(c)
        g.FillRectangle(p, 0, 0, width, height)
        g.FillRectangle(p, width * 2, 0, width, height)

    (width, height), dstBmp

let img user1Avatar user2Avatar perc (outputStream:System.IO.MemoryStream) =
    use dstBmp = new Bitmap(templateBmp.Width, templateBmp.Height)
    use g = Graphics.FromImage dstBmp
    g.DrawImageUnscaled(templateBmp, 0, 0)

    // [very cool gradient](https://stackoverflow.com/a/49321304)
    // but used [this](https://stackoverflow.com/a/2011839)
    let getColor i =
        let max' = Color.FromArgb 0xff00ff00
        let min' = Color.FromArgb 0xffff0000
        let rMax = int max'.R
        let rMin = int min'.R
        let gMax = int max'.G
        let gMin = int min'.G
        let bMax = int max'.B
        let bMin = int min'.B

        let i = float32 i
        let size = 100.0f // [0..100]
        let rAverage = rMin + int (float32 (rMax - rMin) * i / size)
        let gAverage = gMin + int (float32 (gMax - gMin) * i / size)
        let bAverage = bMin + int (float32 (bMax - bMin) * i / size)
        Color.FromArgb(int rAverage, int gAverage, int bAverage)

    g.SmoothingMode <- Drawing2D.SmoothingMode.HighQuality
    g.TextRenderingHint <- Text.TextRenderingHint.AntiAlias
    g.InterpolationMode <- Drawing2D.InterpolationMode.HighQualityBicubic
    g.PixelOffsetMode <- Drawing2D.PixelOffsetMode.HighQuality

    let drawText shift i =
        // let i = 100
        use txtBrush = new SolidBrush(getColor i)
        use fontFamily = new FontFamily("DejaVu Sans Condensed")
        // FontFamily.Families
        // |> Array.find (fun x -> x.Name.Contains "eja")

        let fontSize =
            // 320 66.5
            // 133 x
            // (300. * x) / 320.
            (66.5f * float32 height) / 320.f

        use font = new Font(fontFamily, fontSize)
        let str = sprintf "%d%%" i
        let size =
            g.MeasureString(str, font, 0, StringFormat.GenericTypographic)

        let srcWidth = size.Width
        let srcHeight = size.Height
        let x = float32 width * shift + (float32 width - srcWidth) / 2.f
        let y = (float32 height - srcHeight) / 2.f

        let p = new Drawing2D.GraphicsPath()
        p.AddString(
            str,
            fontFamily,
            (int) FontStyle.Regular,
            (g.DpiY * fontSize / 72.f),
            new PointF(x, y),
            StringFormat.GenericTypographic
        )

        use pen = Pens.Black.Clone() :?> Pen
        pen.Width <-
            (4.f * float32 height) / 320.f

        g.DrawPath(pen, p);
        g.FillPath(txtBrush, p)

    drawText 1.f perc

    g.InterpolationMode <- Drawing2D.InterpolationMode.Bilinear

    // 320 300
    // 133 x
    // (300. * x) / 320.
    let size = (300.f * float32 height) / 320.f

    let f shift (xs:byte array) =
        use imgStream = new System.IO.MemoryStream(xs)
        use srcBmp = new Bitmap(imgStream)

        let srcWidth = size
        let srcHeight = size

        let x = float32 width * shift + (float32 width - srcWidth) / 2.f
        let y = (float32 height - srcHeight) / 2.f

        let r = RectangleF(x, y, srcWidth, srcHeight)
        g.DrawImage(srcBmp, r)

    user1Avatar
    |> Either.iter (f 0.f)

    user2Avatar
    |> Either.iter (f 2.f)

    dstBmp.Save(outputStream, Imaging.ImageFormat.Png)

// let user1Avatar = WebClientDownloader.getData [] "https://cdn.discordapp.com/avatars/796931597898088448/4aa0bed0799253a4bd5b119c355b28f1.png?size=128"
// let user2Avatar = WebClientDownloader.getData [] "https://cdn.discordapp.com/avatars/436300487307034634/6112783a3ddbe8d59e6b6ec7f419b796.png?size=128"
// do
//     let m = new System.IO.MemoryStream()
//     img user1Avatar user2Avatar 100 m
//     m.Position <- 0L
//     use file = new System.IO.FileStream("output.png", System.IO.FileMode.OpenOrCreate)
//     m.WriteTo(file)
