open System

[<AutoOpen>]
module Cin =
    let read f = stdin.ReadLine() |> f
    let reada f = stdin.ReadLine().Split() |> Array.map f

[<EntryPoint>]
let main _ =
    let n = read int
    let ass = reada int
    let rec f i = if i % 2 = 0 then (1 + f (i / 2)) else 0
    ass
    |> Array.map f
    |> Array.min
    |> printfn "%d"
    0
