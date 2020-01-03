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
        let m = ModInt.modulo
        let l, r = ModInt.value2 lhs rhs
        let x = l + r
        match x with
        | _ when x >= m -> ModInt.init (x - m)
        | _ -> ModInt.init x

    static member inline (-) (lhs: ModInt, rhs: ModInt): ModInt =
        let m = ModInt.modulo
        let l, r = ModInt.value2 lhs rhs
        let x = l - r
        match x with
        | _ when x < 0L -> ModInt.init (x + m)
        | _ -> ModInt.init x

    static member inline (*) (lhs: ModInt, rhs: ModInt): ModInt =
        let m = ModInt.modulo
        let l, r = ModInt.value2 lhs rhs
        let x = l * r % m
        ModInt.init x

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

    static member inline (~-) (x: ModInt): ModInt =
        let v = ModInt.value x
        ModInt.init (-v)

type BiCoef =
    { modulo: int64
      fact: int64 array
      inv: int64 array
      finv: int64 array }

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module BiCoef =

    let init (n: int) (modulo: int64): BiCoef =
        let fact = Array.init n (fun _ -> 1L)
        let inv = Array.init n (fun _ -> 1L)
        let finv = Array.init n (fun _ -> 1L)
        let m = modulo |> int
        for i in 2 .. n - 1 do
            let a = (ModInt.init fact.[i - 1]) * (ModInt.init i)
            fact.[i] <- ModInt.value a
            let a = (ModInt.init -inv.[m % i]) * (ModInt.init (m % i))
            inv.[i] <- ModInt.value a
            let a = (ModInt.init finv.[i - 1]) * (ModInt.init inv.[i])
            finv.[i] <- ModInt.value a

        { BiCoef.modulo = modulo
          fact = fact
          inv = inv
          finv = finv }

    let inline com (n: ^a) (k: ^b) (bicoef: BiCoef) =
        let n, k = int n, int k
        match n, k with
        | _, _ when n < k -> 0L
        | _, _ when n < 0 -> 0L
        | _, _ when k < 0 -> 0L
        | _ ->
            let res =
                (ModInt.init bicoef.fact.[n]) * (ModInt.init bicoef.finv.[k]) * (ModInt.init bicoef.finv.[n - k])
            res |> ModInt.value
