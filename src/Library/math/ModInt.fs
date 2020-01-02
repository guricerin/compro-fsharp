namespace Compro.Math

type ModInt(x: int64) =
    static let modulo = (int64 1e9) + 7L

    member __.Value: int64 =
        let y = x % modulo
        match y with
        | _ when y < 0L -> y + modulo
        | _ when y >= modulo -> y - modulo
        | _ -> y

    override __.ToString() = __.Value.ToString()

    override __.Equals(rhs: obj) =
        match rhs with
        | :? ModInt as rhs -> __.Value = rhs.Value
        | _ -> false

    override __.GetHashCode() = __.Value ^^^ modulo |> int

    static member (+) (lhs: ModInt, rhs: ModInt) = ModInt(lhs.Value + rhs.Value)

    static member (-) (lhs: ModInt, rhs: ModInt) = ModInt(lhs.Value - rhs.Value)

    static member (*) (lhs: ModInt, rhs: ModInt) = ModInt(lhs.Value * rhs.Value)

    /// フェルマーの小定理
    /// a / b = a * b^-1 (mod p)
    /// b * b^p-2 = 1 (mod p) -> 両辺に b^-1 を掛けて、b^p-2 = b^-1 (mod p)
    static member (/) (lhs: ModInt, rhs: ModInt) = lhs * (rhs ** (modulo - 2L))

    /// **
    /// 繰り返し二乗法
    static member Pow(n: ModInt, m: int64) =
        let mutable res = ModInt(1L)
        let mutable n = n
        let mutable m = m
        while m > 0L do
            if (m &&& 1L) = 1L then res <- res * n
            n <- n * n
            m <- m >>> 1
        res
