namespace Compro.Math

module Prime =

    let inline isPrime (n: int) =
        let limit =
            n
            |> float
            |> sqrt
            |> int
        seq {
            for p in 2 .. limit do
                if n % p = 0 then yield ()
        }
        |> Seq.isEmpty

    /// nを素因数分解した結果を返す
    let primeFactors (n: int64): Map<int64, int64> =
        let limit =
            n
            |> float
            |> sqrt
            |> int64
        let rec count x p acc =
            if x % p = 0L then count (x / p) p (acc + 1L) else acc

        let mutable n = n

        let res =
            seq {
                for p in 2L .. limit + 1L do
                    let c = count n p 0L
                    if c <> 0L then
                        let div = (float p) ** (float c) |> int64
                        n <- n / div
                        yield (p, c)
            }
            |> Map.ofSeq
        if n = 1L then res else res.Add(n, 1L)

    /// nの約数の個数
    let divisorsCount (n: int64): int64 = primeFactors n |> Map.fold (fun acc k v -> acc * (v + 1L)) 1L

    /// upper以下の素数を列挙
    let sieveToUpper upper =
        seq {
            yield 2
            let knownComposites = System.Collections.Generic.SortedSet<int>()
            for i in 3 .. 2 .. upper do
                let found = knownComposites.Contains(i)
                if not found then yield i
                for j in i .. i .. upper do
                    knownComposites.Add(j) |> ignore
        }

    /// nの約数を列挙(n自身を含む)
    /// O(log n)
    let divisors (n: int64): int64 array =
        let lim =
            n
            |> float
            |> sqrt
            |> int64
        seq {
            for i in 1L .. lim do
                if n % i = 0L then
                    yield i
                    if i * i <> n then yield n / i
        }
        |> Array.ofSeq
        |> Array.sort
