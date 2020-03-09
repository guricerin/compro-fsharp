namespace Compro.Math.ModInt

/// BEGIN CUT HERE
[<Struct>]
type ModInt = MVal of int64

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>] // 型名とモジュール名の重複を許す
module ModInt =
    [<Literal>]
    let Modulo = 1000000007L

    let inline init (x: ^a): ModInt =
        let x = (int64 x) % Modulo
        match x with
        | _ when x < 0L -> MVal(x + Modulo)
        | _ when x >= Modulo -> MVal(x - Modulo)
        | _ -> MVal x

    let zero = init 0
    let one = init 1

    let value (MVal x) = x

    let value2 (x: ModInt) (y: ModInt) = (value x, value y)

    let toString (MVal v): string = sprintf "%d" v

    /// 拡張ユークリッドの互除法
    /// a (mod m) における逆元 a^-1
    let inline inverse (MVal a): ModInt =
        let mutable (a, b, u, v) = (a, Modulo, 1L, 0L)
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
        let x = l + r
        ModInt.init x

    static member inline (-) (lhs: ModInt, rhs: ModInt): ModInt =
        let l, r = ModInt.value2 lhs rhs
        let x = l - r
        ModInt.init x

    static member inline (*) (lhs: ModInt, rhs: ModInt): ModInt =
        let l, r = ModInt.value2 lhs rhs
        let x = l * r
        ModInt.init x

    /// a / b = a * b^-1 (mod m)
    static member inline (/) (lhs: ModInt, rhs: ModInt): ModInt =
        let r = ModInt.inverse rhs
        lhs * r

    /// a^n (mod m) 繰り返しニ乗法
    /// O(log n)
    static member inline Pow(a: ModInt, e: int64): ModInt =
        let mutable (res, a, e) = (ModInt.one, a, e)
        while e > 0L do
            if (e &&& 1L) <> 0L then res <- res * a
            a <- a * a
            e <- e >>> 1
        res

    /// 符号反転
    static member inline (~-) (x: ModInt): ModInt =
        let v = ModInt.value x
        ModInt.init (-v)

type BinomialCoefficient =
    { modulo: int64
      fact: ModInt array
      inv: ModInt array
      finv: ModInt array }

    // nCk
    member inline self.Com(n: int, k: int) =
        let n, k = int n, int k
        let zero = ModInt.zero
        match n, k with
        | _ when n < k -> zero
        | _ when n < 0 -> zero
        | _ when k < 0 -> zero
        | _ ->
            let res = self.fact.[n] * self.finv.[k] * self.finv.[n - k]
            res

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module BinomialCoefficient =

    // nはせいぜい10^6ぐらいが限界
    let inline init (n: int) (modulo: ^a): BinomialCoefficient =
        let one = ModInt.one
        let fact = Array.init n (fun _ -> one)
        let inv = Array.init n (fun _ -> one)
        let finv = Array.init n (fun _ -> one)
        let m = modulo |> int
        for i in 2 .. n - 1 do
            fact.[i] <- fact.[i - 1] * (ModInt.init i)
            inv.[i] <- -inv.[m % i] * (ModInt.init (m / i))
            finv.[i] <- finv.[i - 1] * inv.[i]

        { BinomialCoefficient.modulo = modulo |> int64
          fact = fact
          inv = inv
          finv = finv }
