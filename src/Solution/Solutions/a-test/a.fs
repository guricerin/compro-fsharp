open System

[<AutoOpen>]
module Cin =
    let read f = stdin.ReadLine() |> f
    let reada f = stdin.ReadLine().Split() |> Array.map f

[<EntryPoint>]
let main _ =
    let s = read string
    Seq.sumBy (fun c -> if c = '1' then 1 else 0) s |> printfn "%d"
    0
