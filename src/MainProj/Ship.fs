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

// #r @"C:\Users\User\.nuget\packages\sixlabors.fonts\1.0.0-beta15\lib\netstandard2.1\SixLabors.Fonts.dll"
// #r @"C:\Users\User\.nuget\packages\sixlabors.imagesharp.drawing\1.0.0-beta13\lib\netcoreapp3.1\SixLabors.ImageSharp.Drawing.dll"
// #r @"C:\Users\User\.nuget\packages\sixlabors.imagesharp\1.0.3\lib\netcoreapp3.1\SixLabors.ImageSharp.dll"
// System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats
open SixLabors.ImageSharp.Processing
open SixLabors.ImageSharp.Drawing.Processing

/// blue frame + heart + blue frame
let (width, height), templateBmp =
    let path = @"heart.png"
    // let path = @"heart100.png"
    use heart = Image.Load(path)
    let width, height = 133, 133 // heart.Width, heart.Height
    let dstBmp = new Image<Rgba32>(width * 3, height)

    heart.Mutate (fun ctx ->
        ctx.Resize(width, height)
        |> ignore
    )
    dstBmp.Mutate (fun ctx ->
        ctx.DrawImage(heart, Point(width, 0), new GraphicsOptions())
        |> ignore
    )

    let widthf, heightf = float32 width, float32 height
    do
        let c = Color.FromRgba(0x29uy, 0x80uy, 0xb9uy, 0xffuy)
        dstBmp.Mutate(fun ctx ->
            ctx.Fill(c, RectangleF(0.f, 0.f, widthf, heightf))
            |> ignore
            ctx.Fill(c, RectangleF(widthf * 2.f, 0.f, widthf, heightf))
            |> ignore
        )

    (width, height), dstBmp
// templateBmp.Save("output.png")

let img user1Avatar user2Avatar perc (outputStream:System.IO.MemoryStream) =
    use dstBmp = new Image<Rgba32>(templateBmp.Width, templateBmp.Height)
    dstBmp.Mutate (fun ctx ->
        ctx.DrawImage(templateBmp, new GraphicsOptions ())
        |> ignore
    )

    // [very cool gradient](https://stackoverflow.com/a/49321304)
    // but used [this](https://stackoverflow.com/a/2011839)
    let getColor i =
        let rMax, gMax, bMax = 0x00, 0xff, 00
        let rMin, gMin, bMin = 0xff, 0x00, 0x00

        let i = float32 i
        let size = 100.0f // [0..100]
        let rAverage = rMin + int (float32 (rMax - rMin) * i / size)
        let gAverage = gMin + int (float32 (gMax - gMin) * i / size)
        let bAverage = bMin + int (float32 (bMax - bMin) * i / size)
        Color.FromRgb(byte rAverage, byte gAverage, byte bAverage)

    let drawText shift prec =
        // let i = 100
        let txtBrush = new SolidBrush(getColor prec)
        // https://docs.sixlabors.com/articles/fonts/gettingstarted.html
        let family = SixLabors.Fonts.SystemFonts.Find "DejaVu Sans Condensed"
        // FontFamily.Families
        // |> Array.find (fun x -> x.Name.Contains "eja")

        let fontSize =
            // 320 66.5
            // 133 x
            // (300. * x) / 320.
            (66.5f * float32 height) / 320.f

        let font = new SixLabors.Fonts.Font(family, fontSize)
        let str = sprintf "%d%%" prec

        let options = new DrawingOptions()
        let rendererOptions = new SixLabors.Fonts.RendererOptions(font, options.TextOptions.DpiX, options.TextOptions.DpiY)
        let size =
            SixLabors.Fonts.TextMeasurer.Measure(str, rendererOptions);

        let srcWidth = size.Width
        let srcHeight = size.Height
        let x = float32 width * shift + (float32 width - srcWidth) / 2.f
        let y = (float32 height - srcHeight) / 2.f
        let brush = //Brushes.Horizontal(Color.Red, Color.Blue)
            txtBrush
        let pen = Pens.Solid(Color.Black, (2.f * float32 height) / 320.f)
        dstBmp.Mutate (fun ctx ->
            ctx.DrawText(options, str, font, brush, pen, new PointF(x, y))
            |> ignore
        )

    drawText 1.f perc

    // g.InterpolationMode <- Drawing2D.InterpolationMode.Bilinear

    // 320 300
    // 133 x
    // (300. * x) / 320.
    let size = (300.f * float32 height) / 320.f

    let f shift (xs:byte array) =
        use srcBmp = Image.Load(xs)

        let srcWidth = size
        let srcHeight = size

        let x = float32 width * shift + (float32 width - srcWidth) / 2.f
        let y = (float32 height - srcHeight) / 2.f

        srcBmp.Mutate (fun ctx ->
            ctx.Resize(int srcWidth, int srcHeight)
            |> ignore
        )
        dstBmp.Mutate (fun ctx ->
            ctx.DrawImage(srcBmp, Point(int x, int y), new GraphicsOptions())
            |> ignore
        )

    user1Avatar
    |> Either.iter (f 0.f)

    user2Avatar
    |> Either.iter (f 2.f)

    dstBmp.Save(outputStream, Formats.Png.PngFormat.Instance)

// let user1Avatar = WebClientDownloader.getData [] "https://cdn.discordapp.com/avatars/796931597898088448/4aa0bed0799253a4bd5b119c355b28f1.png?size=128"
// let user2Avatar = WebClientDownloader.getData [] "https://cdn.discordapp.com/avatars/436300487307034634/6112783a3ddbe8d59e6b6ec7f419b796.png?size=128"
// #load "Ship.fs"
// open Ship
// do
//     let m = new System.IO.MemoryStream()
//     img user1Avatar user2Avatar 100 m
//     m.Position <- 0L
//     use file = new System.IO.FileStream("output.png", System.IO.FileMode.OpenOrCreate)
//     m.WriteTo(file)
