namespace Compro.DataStructure

type UnionFind =
    { par: int array
      rank: int array }

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module UnionFind =

    let inline init (n: ^a) =
        let n = int n
        let par = Array.init n id
        let rank = Array.zeroCreate n
        { UnionFind.par = par
          rank = rank }

    let rec root x uf =
        let par = uf.par
        match x = par.[x] with
        | true -> x
        | false ->
            let px = par.[x]
            par.[x] <- root px uf
            par.[x]

    let find x y uf = (root x uf) = (root y uf)

    let unite x y uf =
        let par, rank = uf.par, uf.rank
        let rx, ry = root x uf, root y uf
        match rx = ry with
        | true -> false
        | false ->
            let large, small =
                if rank.[rx] < rank.[ry] then ry, rx else rx, ry
            par.[small] <- large
            rank.[large] <- rank.[large] + rank.[small]
            rank.[small] <- 0
            true
