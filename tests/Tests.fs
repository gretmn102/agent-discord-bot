open Fuchu
open FsharpMyExtension
open FsharpMyExtension.Either

[<EntryPoint;System.STAThread>]
let main arg =
    System.Environment.CurrentDirectory <-
        System.IO.Path.Combine(System.Environment.CurrentDirectory, @"bin/Release/netcoreapp3.1")

    defaultMainThisAssembly arg
