namespace Compro.DataStructure

/// セグメント木:
/// 各ノードが区間に対応づいた完全二分木。モノイドの区間に対する様々な演算が O(log n) で実現可能
/// モノイド:
/// ある集合Sに対して、二項演算(S * S -> S)が与えられて、
/// 結合律( a * b * c = (a * b) * c = a * (b * c) )と、
/// 単位元( a * e = e * a = a を満たすe )が存在するときのS
/// セグメント木の種類
/// 一点更新区間取得: (狭義の)セグメント木
/// 区間更新一点取得: 双対セグメント木
/// 区間更新区間取得: 遅延伝搬セグメント木

/// BEGIN CUT HERE

/// 一点更新区間取得
type SegTree<'Monoid> =
    {
      /// 実データの要素数(葉ノードの数)
      size: int
      height: int
      /// モノイドの単位元
      unity: 'Monoid
      /// 0-indexed
      nodes: 'Monoid array
      /// 二項演算
      merge: Merge<'Monoid> }

and Merge<'a> = 'a -> 'a -> 'a

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SegTree =

    let internal sizeAndHeight n =
        let rec loop sAcc hAcc =
            if sAcc < n then loop (sAcc <<< 1) (hAcc + 1) else (sAcc, hAcc)
        loop 1 0

    let inline internal parent i = (i - 1) / 2
    let inline internal leftChild i = (i <<< 1) + 1
    let inline internal rightChild i = (i <<< 1) + 2
    let inline internal leafIdx tree k = k + tree.size - 1

    let init (n: int) (f: Merge<'Monoid>) (unity: 'Monoid) =
        let size, height = sizeAndHeight n
        let nodes = Array.init (size * 2 - 1) (fun _ -> unity)
        { SegTree.size = size
          height = height
          unity = unity
          nodes = nodes
          merge = f }

    let build (sq: 'Monoid seq) f unity =
        let sq = Array.ofSeq sq
        let len = Array.length sq
        let tree = init len f unity
        let nodes = tree.nodes
        // 葉ノードに値を格納
        for i in 0 .. len - 1 do
            let li = leafIdx tree i
            nodes.[li] <- sq.[i]
        // 上にマージしていく
        for i in tree.size - 2 .. -1 .. 0 do
            let lc, rc = leftChild i, rightChild i
            nodes.[i] <- f nodes.[lc] nodes.[rc]
        tree

    /// 一点更新
    let update k x tree: unit =
        let k = leafIdx tree k
        let nodes = tree.nodes
        nodes.[k] <- x
        // 子から親に伝搬
        let mutable p = k
        while p > 0 do
            p <- parent p
            let lc, rc = leftChild p, rightChild p
            nodes.[p] <- tree.merge nodes.[lc] nodes.[rc]

    let rec internal queryCore (a: int) (b: int) (k: int) (l: int) (r: int) tree: 'Monoid =
        // 区間外
        if r <= a || b <= l then
            tree.unity
        // 完全被覆
        elif a <= l && r <= b then
            tree.nodes.[k]
        // 一部だけ被覆
        else
            let lc, rc, mid = leftChild k, rightChild k, (l + r) / 2
            let lv = queryCore a b lc l mid tree
            let rv = queryCore a b rc mid r tree
            tree.merge lv rv

    let query a b tree: 'Monoid = queryCore a b 0 0 tree.size tree

    let get k tree =
        let k = leafIdx tree k
        tree.nodes.[k]

/// END CUT HERE
