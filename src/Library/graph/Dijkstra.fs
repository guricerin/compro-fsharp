namespace Compro.Graph.Dijkstra

open System
open Compro.Graph.GraphUnit
open Compro.DataStructure.BinaryHeap
open Compro.Math.ModInt

/// ダイクストラ法(単一始点最短路)
/// 負辺のない単一始点全点間最短路を求める

/// BEGIN CUT HERE
[<RequireQualifiedAccess>]
module Dijkstra =

    /// ダイクストラ基本形
    /// 頂点startからの全点間最短路配列を返す
    /// 到達できないノードにはinfが格納される
    /// O(E log V)
    let inline dijkstra (graph: WeightedGraph< ^a >) (startNode: int) (inf: ^a): ^a array =
        let nedge = graph |> Array.length // ノード数
        let dist = Array.init nedge (fun _ -> inf) // 始点からの距離
        dist.[startNode] <- LanguagePrimitives.GenericZero
        let heap = BinaryHeap<Edge<'a>>(Edge.less) // コストの低い順に探索する
        let start = Edge.initWithoutFrom startNode dist.[startNode]
        heap.Push(start)
        while heap.Any() do
            let from = heap.Pop()
            match dist.[from.toward] < from.cost with
            | true -> ()
            | _ ->
                for edge in graph.[from.toward] do
                    let nextCost = edge.cost + from.cost
                    if dist.[edge.toward] > nextCost then
                        dist.[edge.toward] <- nextCost
                        let nextEdge = Edge.initWithoutFrom edge.toward dist.[edge.toward]
                        heap.Push(nextEdge)
        dist

    /// 最短経路数の数え上げ: https://drken1215.hatenablog.com/entry/2018/02/09/003200
    let inline withShortestPathNum (graph: WeightedGraph< ^a >) (startNode: int) (inf: ^a) =
        let nedge = graph |> Array.length
        // dist[v]: 始点(startNode)から頂点vへの最短経路長
        let dist = Array.init nedge (fun _ -> inf)
        dist.[startNode] <- LanguagePrimitives.GenericZero
        // nums[v]: 始点(startNode)から頂点vへの最短経路数
        let nums = Array.init nedge (fun _ -> ModInt.zero)
        nums.[startNode] <- ModInt.one
        let heap = BinaryHeap<Edge<'a>>(Edge.less)
        let start = Edge.initWithoutFrom startNode dist.[startNode]
        heap.Push(start)
        while heap.Any() do
            let from = heap.Pop()
            match dist.[from.toward] < from.cost with
            | true -> ()
            | _ ->
                for edge in graph.[from.toward] do
                    let nextCost = edge.cost + from.cost
                    if dist.[edge.toward] = nextCost then
                        nums.[edge.toward] <- nums.[edge.toward] + nums.[edge.from]
                    elif dist.[edge.toward] > nextCost then
                        dist.[edge.toward] <- nextCost
                        nums.[edge.toward] <- nums.[edge.from]
                        let nextEdge = Edge.initWithoutFrom edge.toward dist.[edge.toward]
                        heap.Push(nextEdge)

        dist, nums
