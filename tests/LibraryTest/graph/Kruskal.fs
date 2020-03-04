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
