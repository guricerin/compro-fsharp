namespace Compro.DataStructure

/// 素集合データ構造: UnionFindTree (DisjointSet)
/// グループを1つの木で表現。全体としては森

/// BEGIN CUT HERE
type UnionFind =
    {
      /// 添字iが属するグループID (0-indexed)
      par: int array
      /// 各集合の要素数
      size: int array }

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module UnionFind =

    /// O(n)
    let init (n: int): UnionFind =
        let par = Array.init n id
        let size = Array.init n (fun _ -> 1)
        { UnionFind.par = par
          size = size }

    /// xの先祖(xが属するグループID)
    let rec root (x: int) (uf: UnionFind): int =
        let par = uf.par
        match x = par.[x] with
        | true -> x
        | false ->
            let px = par.[x]
            par.[x] <- root px uf
            par.[x]

    /// 連結判定
    /// ならし O(α(n))
    let find (x: int) (y: int) (uf: UnionFind) = (root x uf) = (root y uf)

    /// xとyを同じグループに併合
    /// ならし O(α(n))
    let unite (x: int) (y: int) (uf: UnionFind): bool =
        let par, size = uf.par, uf.size
        let rx, ry = root x uf, root y uf
        match rx = ry with
        | true -> false // 既に同じグループ
        | _ ->
            // マージテク(大きい方に小さい方を併合)
            let large, small =
                if size.[rx] < size.[ry] then ry, rx else rx, ry
            par.[small] <- large
            size.[large] <- size.[large] + size.[small]
            size.[small] <- 0
            true

    /// xが属する素集合の要素数
    /// O(1)
    let size (x: int) (uf: UnionFind): int =
        let rx = root x uf
        uf.size.[rx]

    /// 連結成分の個数
    /// O(n)
    let treeNum (uf: UnionFind): int =
        let par = uf.par
        let mutable cnt = 0
        par
        |> Array.iteri (fun i x ->
            if i = x then cnt <- cnt + 1)
        cnt
