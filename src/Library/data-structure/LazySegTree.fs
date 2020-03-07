namespace Compro.DataStructure.LazySegTree

/// 遅延セグメント木:
/// 連続する区間に対して更新と取得が可能となったセグメント木
/// 参考: http://tsutaj.hatenablog.com/entry/2017/03/30/224339
/// https://github.com/ei1333/library/blob/master/structure/segment-tree/lazy-segment-tree.cpp

/// BEGIN CUT HERE

/// 区間更新区間取得
type LazySegTree<'Monoid, 'OpMonoid when 'OpMonoid: equality> =
    {
      /// 実データの要素数(葉ノードの数)
      size: int
      /// 木の高さ
      height: int
      /// モノイドの単位元
      unity: 'Monoid
      /// 作用素の単位元
      opUnity: 'OpMonoid
      /// 0-indexed
      nodes: 'Monoid array
      lazyNodes: 'OpMonoid array
      /// 要素をマージする関数
      merge: Merge<'Monoid>
      /// 要素に作用素を適用する関数
      apply: Apply<'Monoid, 'OpMonoid>
      /// 作要素をマージする関数
      opMerge: Merge<'OpMonoid> }

and Merge<'a> = 'a -> 'a -> 'a

and Apply<'a, 'b> = 'a -> 'b -> 'a

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module LazySegTree =

    let inline internal sizeAndHeight n =
        let rec loop sAcc hAcc =
            if sAcc < n then loop (sAcc <<< 1) (hAcc + 1) else (sAcc, hAcc)
        loop 1 0

    let inline internal parent i = (i - 1) / 2
    let inline internal leftChild i = (i <<< 1) + 1
    let inline internal rightChild i = (i <<< 1) + 2
    let inline internal leafIdx tree k = k + tree.size - 1

    let inline init (n: int) (unity: 'Monoid) (opUnity: 'OpMonoid) f g h: LazySegTree<_, _> =
        let size, height = sizeAndHeight n
        let nodes = Array.init (size * 2 - 1) (fun _ -> unity)
        let lazyNodes = Array.init (size * 2 - 1) (fun _ -> opUnity)
        { LazySegTree.size = size
          height = height
          unity = unity
          opUnity = opUnity
          nodes = nodes
          lazyNodes = lazyNodes
          merge = f
          apply = g
          opMerge = h }

    let inline build (sq: 'Monoid seq) mUnity omUnity f g h =
        let sq = Array.ofSeq sq
        let len = Array.length sq
        let tree = init len mUnity omUnity f g h
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

    /// k番目のノードについて遅延評価を行う
    let inline propagate (k: int) tree: unit =
        let nodes, lazyNodes = tree.nodes, tree.lazyNodes
        let opMerge, apply = tree.opMerge, tree.apply
        // 作用素の単位元があるのに条件分岐させる理由
        // => 作用が単なる上書き(MonoidとOpMonoidの型が一致しており、apply(a,b) -> b)の場合にバグるから
        if lazyNodes.[k] <> tree.opUnity then
            // 葉ノードではない場合、子ノードに作用素を伝搬
            if k < tree.size - 1 then
                let lc, rc = leftChild k, rightChild k
                lazyNodes.[lc] <- opMerge lazyNodes.[lc] lazyNodes.[k]
                lazyNodes.[rc] <- opMerge lazyNodes.[rc] lazyNodes.[k]
            nodes.[k] <- apply nodes.[k] lazyNodes.[k]
            lazyNodes.[k] <- tree.opUnity

    let rec internal update a b (x: 'OpMonoid) k l r tree: unit =
        do propagate k tree
        let nodes, lazyNodes = tree.nodes, tree.lazyNodes
        if b <= l || r <= a then
            ()
        elif a <= l && r <= b then
            lazyNodes.[k] <- tree.opMerge lazyNodes.[k] x
            do propagate k tree
        else
            let lc, rc, mid = leftChild k, rightChild k, (l + r) / 2
            do update a b x lc l mid tree
               update a b x rc mid r tree
            nodes.[k] <- tree.merge nodes.[lc] nodes.[rc]

    let rec internal fold (a: int) (b: int) (k: int) (l: int) (r: int) tree: 'Monoid =
        do propagate k tree
        // 区間外
        if b <= l || r <= a then
            tree.unity
        // 完全被覆
        elif a <= l && r <= b then
            tree.nodes.[k]
        // 一部だけ被覆
        else
            let lc, rc, mid = leftChild k, rightChild k, (l + r) / 2
            let lm = fold a b lc l mid tree
            let rm = fold a b rc mid r tree
            tree.merge lm rm


type LazySegTree<'Monoid, 'OpMonoid when 'OpMonoid: equality> with

    /// 半開区間[l, r)を更新
    /// O(log n)
    member self.Update(l: int, r: int, x: 'OpMonoid): unit = LazySegTree.update l r x 0 0 self.size self

    /// 半開区間[l, r)の演算結果を取得
    /// O(log n)
    member self.Fold(l: int, r: int): 'Monoid = LazySegTree.fold l r 0 0 self.size self

    /// O(log n)
    member self.At(k: int) = self.Fold(k, k + 1)
