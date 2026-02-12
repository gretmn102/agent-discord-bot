open Fuchu
open FsharpMyExtension
open FsharpMyExtension.Either

[<EntryPoint;System.STAThread>]
let main arg =
    // Use the base directory of the test assembly (contains .env, etc.)
    System.Environment.CurrentDirectory <-
        System.AppContext.BaseDirectory.TrimEnd('/', '\\')

    defaultMainThisAssembly arg
