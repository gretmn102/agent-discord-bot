// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------
#r "paket: groupref build //"
#load "./.fake/build.fsx/intellisense.fsx"
#r "netstandard"

open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators
// --------------------------------------------------------------------------------------
// Build variables
// --------------------------------------------------------------------------------------
let f projName =
    let pattern = sprintf @"**\%s.fsproj" projName
    let xs = !! pattern
    xs
    |> Seq.tryExactlyOne
    |> Option.defaultWith (fun () ->
        xs
        |> List.ofSeq
        |> failwithf "'%s' expected exactly one but:\n%A" pattern
    )

let testProjName = "Tests"
let testProjPath = sprintf "Tests\\%s.fsproj" testProjName

let mainProjName = "MainProj"
let mainProjPath = f mainProjName
let mainProjDir =
    Fake.IO.Path.getDirectory mainProjPath

let deployDir = Path.getFullName "./deploy"
// --------------------------------------------------------------------------------------
// Helpers
// --------------------------------------------------------------------------------------
open Fake.DotNet
let buildConf = DotNet.BuildConfiguration.Release
// --------------------------------------------------------------------------------------
// Targets
// --------------------------------------------------------------------------------------
let removeSQLiteInteropDll projPath =
    let dir = Fake.IO.Path.getDirectory projPath
    let localpath = sprintf "bin/%A/netcoreapp3.1/SQLite.Interop.dll" buildConf
    let path = Fake.IO.Path.combine dir localpath
    Fake.IO.File.delete path

Target.create "Clean" (fun _ -> Shell.cleanDir deployDir)

Target.create "BuildMain" (fun _ ->
    mainProjDir
    |> DotNet.build (fun x ->
        { x with Configuration = buildConf }
    )

    // uncommented if you use SQLiteInterop library
    // removeSQLiteInteropDll mainProjPath
)

Target.create "BuildTest" (fun _ ->
    testProjPath
    |> Fake.IO.Path.getDirectory
    |> DotNet.build (fun x ->
        { x with Configuration = buildConf }
    )

    // uncommented if you use SQLiteInterop library
    // removeSQLiteInteropDll mainProjPath
    // removeSQLiteInteropDll testProjPath
)

let dotnet cmd workingDir =
    let result = DotNet.exec (DotNet.Options.withWorkingDirectory workingDir) cmd ""
    if result.ExitCode <> 0 then failwithf "'dotnet %s' failed in %s" cmd workingDir

Target.create "RunMain" (fun _ ->
    mainProjDir
    |> dotnet "run -c Release"
)

Target.create "RunMainTest" (fun _ ->
    mainProjDir
    |> dotnet "run -c TestBot"
)

Target.create "RunTests" (fun _ ->
    testProjPath
    |> Fake.IO.Path.getDirectory
    |> dotnet "run -c Release"
)

let deploy () =
    let runtimeOption = if Environment.isUnix then "--runtime linux-x64" else ""
    dotnet (sprintf "publish -c Release -o \"%s\" %s" deployDir runtimeOption) mainProjDir

Target.create "Deploy" (fun _ -> deploy())

// --------------------------------------------------------------------------------------
// Build order
// --------------------------------------------------------------------------------------
open Fake.Core.TargetOperators

"Clean"
  ==> "Deploy"

Target.runOrDefault "Deploy"