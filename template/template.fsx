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

module Util =
    let strRev s =
        s
        |> Seq.rev
        |> Seq.map string
        |> String.concat ""

    let inline roundup (a: ^t) (b: ^t): ^t =
        let one = LanguagePrimitives.GenericOne
        (a + b - one) / b

[<EntryPoint>]
let main _ =
    printfn "hello"
    0 // return an integer exit code
