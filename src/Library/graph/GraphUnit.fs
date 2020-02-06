namespace Compro.Graph.GraphUnit

/// BEGIN CUT HERE

/// グラフにおける重み付きの辺
type Edge<'T when 'T: comparison> =
    { from: int
      toward: int
      cost: 'T }

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Edge =
    open System

    let init (from: int) (toward: int) (cost: 'a): Edge<'a> =
        { Edge.from = from
          toward = toward
          cost = cost }

    let initWithoutFrom (toward: int) (cost: 'a): Edge<'a> = init -1 toward cost

    /// 昇順
    let less (x: Edge<'a>) (y: Edge<'a>) = (x.cost :> IComparable<_>).CompareTo(y.cost)

    /// 降順
    let greater (x: Edge<'a>) (y: Edge<'a>) = (y.cost :> IComparable<_>).CompareTo(x.cost)

/// 重み付き辺集合
type Edges<'a when 'a: comparison> = ResizeArray<Edge<'a>>

/// 重み付きグラフ
/// g.[a] => ノードaに直接繋がっている辺群(隣接リスト)
type WeightedGraph<'a when 'a: comparison> = Edges<'a> array

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module WeightedGraph =
    let inline init (n: int): WeightedGraph<'a> = Array.init n (fun _ -> ResizeArray<Edge<'a>>())

/// 重みなしグラフ
/// g.[a] => ノードaと辺で直接繋がっているノード群(隣接リスト)
/// コストは一律 0 or 1
type UnWeightedGraph = ResizeArray<int> array

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module UnWeightedGraph =
    let inline init (n: int): UnWeightedGraph = Array.init n (fun _ -> ResizeArray<int>())
