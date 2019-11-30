#r "paket:
nuget Fake.DotNet.Cli
nuget Fake.IO.FileSystem
nuget Fake.Core.Target //"
#load ".fake/build.fsx/intellisense.fsx"

open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators

Target.initEnvironment()

Target.create "Clean" (fun _ -> !!"src/Library/bin" ++ "src/Library/obj" |> Shell.cleanDirs)

Target.create "Build" (fun _ -> !!"src/Library/*.**proj" |> Seq.iter (DotNet.build id))

Target.create "All" ignore

"Clean" ==> "Build" ==> "All"

Target.runOrDefault "All"
