namespace Compro.Graph.Kruskal

open System
open Compro.Graph.GraphUnit
open Compro.DataStructure.UnionFind

/// 全域木（Spanning Tree）:
/// あるグラフのずべての頂点と、そのグラフを構成する辺の一部のみで構成される木
/// 最小全域木（Minimum Spanning Tree）:
/// 全域木のうち、その辺群のコストの総和が最小になる木

/// BEGIN CUT HERE
[<RequireQualifiedAccess>]
module Kruskal =

    /// クラスカル法基本形
    /// n: 頂点数
    /// O(E log V)
    let inline main (edges: Edges< ^a >) (n: int) =
        // 辺をコストが小さい順に見ていく貪欲法
        edges.Sort(Edge.less)
        let uni = UnionFind.init n
        let mutable res = LanguagePrimitives.GenericZero
        for e in edges do
            // 辺を追加した場合に閉路ができなければ、その辺を採用
            if uni.Unite(e.from, e.toward) then res <- res + e.cost
        res
