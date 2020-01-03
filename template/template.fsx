open System
open System.Collections.Generic

[<AutoOpen>]
module Cin =
    let read f = stdin.ReadLine() |> f
    let reada f = stdin.ReadLine().Split() |> Array.map f

    let readInts() =
        read string
        |> Seq.toArray
        |> Array.map (fun x -> Convert.ToInt32(x.ToString()))

let writer = new IO.StreamWriter(new IO.BufferedStream(Console.OpenStandardOutput()))
let write (s: string) = writer.Write s
let writeln (s: string) = writer.WriteLine s

module Util =
    let strRev (s: string): string =
        s
        |> Seq.rev
        |> Seq.map string
        |> String.concat ""

    let inline roundup (x: ^a) (y: ^a): ^a =
        let one = LanguagePrimitives.GenericOne
        (x + y - one) / y

let solve() =
    printfn ""
    ()

[<EntryPoint>]
let main _ =
    solve()
    writer.Close()
    0 // return an integer exit code
