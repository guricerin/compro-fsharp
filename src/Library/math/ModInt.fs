namespace Compro.Math

type ModInt = MVal of int64

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>] // 型名とモジュール名の重複を許す
module ModInt =
    let modulo = (int64 1e9) + 7L

    let inline init (x: ^a): ModInt =
        let x = (int64 x) % modulo
        match x with
        | _ when x < 0L -> MVal(x + modulo)
        | _ when x >= modulo -> MVal(x - modulo)
        | _ -> MVal x

    let value (MVal x) = x

    let value2 (x: ModInt) (y: ModInt) = (value x, value y)

    let toString (MVal v): string = sprintf "%d" v

    /// 拡張ユークリッドの互除法
    /// a (mod m) における逆元 a^-1
    let inline inverse (MVal a): ModInt =
        let mutable (a, b, u, v) = (a, modulo, 1L, 0L)
        while b > 0L do
            let t = a / b
            a <- a - (t * b)
            let tmp = a
            a <- b
            b <- tmp
            u <- u - (t * v)
            let tmp = u
            u <- v
            v <- tmp
        init u

type ModInt with

    static member inline (+) (lhs: ModInt, rhs: ModInt): ModInt =
        let l, r = ModInt.value2 lhs rhs
        ModInt.init (l + r)

    static member inline (-) (lhs: ModInt, rhs: ModInt): ModInt =
        let l, r = ModInt.value2 lhs rhs
        ModInt.init (l - r)

    static member inline (*) (lhs: ModInt, rhs: ModInt): ModInt =
        let l, r = ModInt.value2 lhs rhs
        ModInt.init (l * r)

    /// a / b = a * b^-1 (mod m)
    static member inline (/) (lhs: ModInt, rhs: ModInt): ModInt =
        let r = ModInt.inverse rhs
        lhs * r

    /// a^n (mod m) 繰り返しニ乗法
    /// O(log n)
    static member inline Pow(a: ModInt, n: int64): ModInt =
        let mutable (res, a, n) = (ModInt.init 1, a, n)
        while n > 0L do
            if (n &&& 1L) = 1L then res <- res * a
            a <- a * a
            n <- n >>> 1
        res
