namespace Compro.Graph.Dijkstra

open System
open Compro.Graph.GraphUnit
open Compro.DataStructure.PriorityQueue

/// ダイクストラ法(単一始点最短路)
/// 負辺のない単一始点全点間最短路を求める

/// BEGIN CUT HERE
[<RequireQualifiedAccess>]
module Dijkstra =

    /// ダイクストラ基本形
    /// 頂点startからの全点間最短路配列を返す
    /// 到達できないノードにはinfが格納される
    /// O(E log V)
    let inline main (graph: WeightedGraph< ^a >) (startNode: int) (inf: ^a): ^a array =
        let nedge = graph |> Array.length // ノード数
        let dist = Array.init nedge (fun _ -> inf) // 始点からの距離
        dist.[startNode] <- LanguagePrimitives.GenericZero
        let heap = PriorityQueue<Edge<'a>>(Edge.less) // コストの低い順に探索する
        let start = Edge.initWithoutFrom startNode dist.[startNode]
        heap.Enque(start)
        while heap.Any() do
            let from = heap.Deque()
            match dist.[from.toward] < from.cost with
            | true -> ()
            | _ ->
                for edge in graph.[from.toward] do
                    let nextCost = edge.cost + from.cost
                    if dist.[edge.toward] <= nextCost then
                        ()
                    else
                        dist.[edge.toward] <- nextCost
                        let nextEdge = Edge.initWithoutFrom edge.toward dist.[edge.toward]
                        heap.Enque(nextEdge)
        dist
