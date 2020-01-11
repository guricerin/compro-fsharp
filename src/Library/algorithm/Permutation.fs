namespace Compro.Algorithm

module Permutation =

    /// 引数が破壊的に更新されることに注意
    let nextPermutation (arr: 'a array): bool =
        let swap i j =
            let tmp = arr.[i]
            arr.[i] <- arr.[j]
            arr.[j] <- tmp

        let mutable r = Array.length arr - 1
        while r > 0 && arr.[r - 1] >= arr.[r] do
            r <- r - 1
        if r <= 0 then
            false
        else
            let mutable l = Array.length arr - 1
            while (arr.[l] <= arr.[r - 1]) do
                l <- l - 1
            if l < r then failwith "nextPermutation: Bug Found!"
            swap l (r - 1)
            let mutable l = Array.length arr - 1
            while r < l do
                swap r l
                r <- r + 1
                l <- l - 1
            true

    /// 順列を列挙
    /// O (n!)
    let permutations (arr: 'a array): seq<'a array> =
        seq {
            yield arr
            let mutable arr = arr |> Array.copy
            while nextPermutation arr do
                yield arr
                arr <- arr |> Array.copy
        }
