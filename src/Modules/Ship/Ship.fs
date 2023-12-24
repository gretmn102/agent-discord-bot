module Ship.Core
open FsharpMyExtension
open FsharpMyExtension.Either
open DiscordBotExtensions.Types

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
    let path = "Modules/Ship/heart.png"
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

// https://docs.sixlabors.com/articles/fonts/gettingstarted.html
let collection = new SixLabors.Fonts.FontCollection()
let fontFamily = collection.Install("Fonts/DejaVuSansCondensed.ttf")

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

        let fontSize =
            (80f * float32 height) / 320.f

        let font = new SixLabors.Fonts.Font(fontFamily, fontSize)
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

let roundImg (dstSrc:Image) =
    let size = float32 dstSrc.Width / 2.f
    let circle = Drawing.EllipsePolygon(size, size, size)
    use mask = new Image<Rgba32>(dstSrc.Width, dstSrc.Width)
    mask.Mutate (fun ctx ->
        let ctx = ctx.Fill(Color.Black)

        let c = Color.White
        let opt = DrawingOptions()
        opt.GraphicsOptions.AlphaCompositionMode <- PixelAlphaCompositionMode.Xor
        ctx.Fill(opt, c, circle)
        |> ignore
    )
    // mask.Save("output.png")
    dstSrc.Mutate (fun ctx ->
        let opt = GraphicsOptions()
        opt.AlphaCompositionMode <- PixelAlphaCompositionMode.DestOut
        ctx.DrawImage(mask, opt)
        |> ignore
    )
let test () =
    let userAvatar =
        WebClientDownloader.getData [] "https://cdn.discordapp.com/avatars/877961992005111818/a5d11850634fa5c22e8e70163f834e26.png?size=128"
        |> Either.get
    use x = Image<Rgba32>.Load(userAvatar)
    roundImg x
    x.Save("output.png")

let massShip (avatars:byte [] []) (outputStream:System.IO.MemoryStream) =
    // [very cool gradient](https://stackoverflow.com/a/49321304)
    // but used [this](https://stackoverflow.com/a/2011839)
    let getColor i =
        let rMax, gMax, bMax = 0xff, 0x00, 0x00
        let rMin, gMin, bMin = 0x00, 0x00, 0xff

        let i = float32 i
        let size = 100.0f // [0..100]
        let rAverage = rMin + int (float32 (rMax - rMin) * i / size)
        let gAverage = gMin + int (float32 (gMax - gMin) * i / size)
        let bAverage = bMin + int (float32 (bMax - bMin) * i / size)
        Color.FromRgb(byte rAverage, byte gAverage, byte bAverage)

    let piDiv180 = System.Math.PI / 180.0
    let circle (cx, cy) r =
        (fun t ->
            let t = t * piDiv180
            r * cos(t) + cx, r * sin(t) + cy)

    let polygon (x, y) r n =
        let n = float n
        let size = 360. / n
        [|0.0..size..359.99|]
        // |> Array.map (fun x -> x + 90.) // inverted pentagram
        |> Array.map (fun x -> x - 90.)
        |> Array.map (circle (x, y) r)

    let canvasSize = 1000
    let width, height = canvasSize, canvasSize
    let avatarSize = 128. + 1. // должен быть четным
    let points =
        let sizeF = (float canvasSize / 2.)
        polygon (sizeF, sizeF) (sizeF - (avatarSize / 2.)) avatars.Length
        |> Array.map (fun (x, y) -> PointF(float32 x, float32 y))
    let combPoints =
        points
        |> List.ofArray
        |> Combinatorics.Comb.comb 2
        |> LazyTree.unpack

    let r = System.Random()

    use dstImg = new Image<Rgba32>(width, height)
    dstImg.Mutate (fun ctx ->
        combPoints
        |> Seq.iter (fun points ->
            let perc = r.Next(0, 101)
            if perc > 0 then
                let c = getColor perc
                let thickness = 10.f
                ctx.DrawLines(c, float32 perc * thickness / 100.f, Array.ofList points)
                |> ignore
        )
    )

    let drawAvatars avatars (img:Image<Rgba32>) =
        img.Mutate (fun ctx ->
            avatars
            |> Array.iteri (fun i (bytes:byte []) ->
                match bytes with
                | [||] -> ()
                | bytes ->
                    use avatarImg = Image.Load bytes
                    roundImg avatarImg
                    let avatarSize = int avatarSize
                    avatarImg.Mutate (fun x -> x.Resize(avatarSize, avatarSize) |> ignore)

                    let f size fn =
                        int (fn points.[i] - float32 size / 2.f + 1.f)
                    // ctx.DrawImage(avatarImg, Point(f avatarImg.Width (fun x -> x.X), f avatarImg.Height (fun x -> x.Y)), new GraphicsOptions())
                    ctx.DrawImage(avatarImg, Point(f avatarSize (fun x -> x.X), f avatarSize (fun x -> x.Y)), new GraphicsOptions())
                    |> ignore
            )
        )
    drawAvatars avatars dstImg

    dstImg.Save(outputStream, Formats.Png.PngFormat.Instance)

// let user1Avatar = WebClientDownloader.getData [] "https://cdn.discordapp.com/avatars/796931597898088448/4aa0bed0799253a4bd5b119c355b28f1.png?size=128"
// let user2Avatar = WebClientDownloader.getData [] "https://cdn.discordapp.com/avatars/436300487307034634/6112783a3ddbe8d59e6b6ec7f419b796.png?size=128"
// let user3Avatar = WebClientDownloader.getData [] "https://discord.com/assets/1f0bfc0865d324c2587920a7d80c609b.png"
// let user4Avatar = WebClientDownloader.getData [] "https://discord.com/assets/3c6ccb83716d1e4fb91d3082f6b21d77.png"
// let userAvatar = WebClientDownloader.getData [] "https://cdn.discordapp.com/avatars/877961992005111818/a5d11850634fa5c22e8e70163f834e26.png?size=128"
// #load "Ship.fs"
// open Ship
// [|
//     user1Avatar
//     user1Avatar
//     user1Avatar
//     user2Avatar
//     user2Avatar
//     user3Avatar
//     user4Avatar
// |]
// |> Array.map Either.get
// |> massShip
