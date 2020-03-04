module Compro.Test.Graph.Kruskal

open Expecto
open Compro.Graph.GraphUnit
open Compro.Graph.Kruskal

[<Tests>]
let ``AOJ GRL 2_A Sample`` =
    test "AOJ GRL 2_A" {
        let v, e = 6, 9
        let edges = Edges.init v

        let ls =
            [ Edge.init 0 1 1
              Edge.init 0 2 3
              Edge.init 1 2 1
              Edge.init 1 3 7
              Edge.init 2 4 1
              Edge.init 1 4 3
              Edge.init 3 4 1
              Edge.init 3 5 1
              Edge.init 4 5 6 ]
        for l in ls do
            edges.Add(l)
        let actual = Kruskal.main edges v
        let expect = 5
        Expect.equal actual expect "5"
    }

[<Tests>]
let ``ABC065 D`` =
    test "ABC065 D" {
        let n = 6
        let xs = [| 8; 4; 12; 18; 13; 7 |]
        let ys = [| 3; 9; 19; 1; 5; 6 |]
        let edges = Edges.init 0
        let ids = [| 0 .. n - 1 |]
        // 座標でソートすることで、ソート後の隣の番号同士はグラフの中でx座標が最も近いノード同士であることを表現
        let xid = ids |> Array.sortBy (fun i -> xs.[i])
        for i in 0 .. n - 2 do
            let u, v = xid.[i], xid.[i + 1]

            let cost =
                xs.[u] - xs.[v]
                |> abs
                |> int64

            let e = Edge.init u v cost
            edges.Add(e)
        let yid = ids |> Array.sortBy (fun i -> ys.[i])
        for i in 0 .. n - 2 do
            let u, v = yid.[i], yid.[i + 1]

            let cost =
                ys.[u] - ys.[v]
                |> abs
                |> int64

            let e = Edge.init u v cost
            edges.Add(e)

        let actual = Kruskal.main edges n
        let expect = 8L
        Expect.equal actual expect ""
    }
