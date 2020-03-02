namespace Compro.DataStructure.UnionFind

/// 素集合データ構造: UnionFindTree (DisjointSet)
/// グループを1つの木で表現。全体としては森

/// BEGIN CUT HERE
type UnionFind =
    {
      /// 添字iが属するグループID (0-indexed)
      par: int array
      /// 各集合の要素数
      size: int array }

    /// xの先祖(xが属するグループID)
    member self.Root(x: int) =
        let par = self.par

        let rec loop x =
            match x = par.[x] with
            | true -> x
            | false ->
                let px = par.[x]
                par.[x] <- loop px
                par.[x]
        loop x

    /// 連結判定
    /// ならし O(α(n))
    member self.Find(x: int, y: int) = self.Root(x) = self.Root(y)

    /// xとyを同じグループに併合
    /// ならし O(α(n))
    member self.Unite(x: int, y: int): bool =
        let par, size = self.par, self.size
        let rx, ry = self.Root(x), self.Root(y)
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
    member self.Size(x: int): int =
        let rx = self.Root(x)
        self.size.[rx]

    /// 連結成分の個数
    /// O(n)
    member self.TreeNum: int =
        let par = self.par
        let mutable cnt = 0
        par
        |> Array.iteri (fun i x ->
            if i = x then cnt <- cnt + 1)
        cnt

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module UnionFind =

    /// O(n)
    let init (n: int): UnionFind =
        let par = Array.init n id
        let size = Array.init n (fun _ -> 1)
        { UnionFind.par = par
          size = size }
