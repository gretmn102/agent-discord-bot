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
    let pattern = sprintf @"**/%s.fsproj" projName
    let xs = !! pattern
    xs
    |> Seq.tryExactlyOne
    |> Option.defaultWith (fun () ->
        xs
        |> List.ofSeq
        |> failwithf "'%s' expected exactly one but:\n%A" pattern
    )

let testProjName = "tests"
let testProjPath =
    Path.combine
        "tests"
        (sprintf "%s.fsproj" testProjName)

let mainProjName = "MainProj"
let mainProjPath = f mainProjName
let mainProjDir =
    Fake.IO.Path.getDirectory mainProjPath

let deployDir = Path.getFullName "./deploy"

let envPath = ".env"
// --------------------------------------------------------------------------------------
// Helpers
// --------------------------------------------------------------------------------------
open Fake.DotNet
let buildConf = DotNet.BuildConfiguration.Release

let dotnet cmd workingDir =
    let result = DotNet.exec (DotNet.Options.withWorkingDirectory workingDir) cmd ""
    if result.ExitCode <> 0 then failwithf "'dotnet %s' failed in %s" cmd workingDir

// --------------------------------------------------------------------------------------
// Targets
// --------------------------------------------------------------------------------------
let removeSQLiteInteropDll projPath =
    let dir = Fake.IO.Path.getDirectory projPath
    let localpath = sprintf "bin/%A/net8.0/SQLite.Interop.dll" buildConf
    let path = Fake.IO.Path.combine dir localpath
    Fake.IO.File.delete path

Target.create "Clean" (fun _ -> Shell.cleanDir deployDir)

let fsharpMyExtProj = "src/FsharpMyExtension/FsharpMyExtension.fsproj"
let nugetLocalDir = "./nuget-local"

Target.create "PackFsharpMyExtension" (fun _ ->
    Directory.ensure nugetLocalDir
    dotnet (sprintf "pack %s -c Release -o %s /p:PackageId=FSharpMyExt /p:Version=2.0.0" fsharpMyExtProj nugetLocalDir) "."
)

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

Target.create "CopyEnvToDeploy" (fun _ ->
    if File.exists envPath then
        Shell.copyFile (Path.combine deployDir envPath) envPath
)

// --------------------------------------------------------------------------------------
// Build order
// --------------------------------------------------------------------------------------
open Fake.Core.TargetOperators

"PackFsharpMyExtension" ==> "BuildMain"
"PackFsharpMyExtension" ==> "BuildTest"

"Clean"
  ==> "PackFsharpMyExtension"
  ==> "Deploy"
  ==> "CopyEnvToDeploy"

Target.runOrDefault "CopyEnvToDeploy"
