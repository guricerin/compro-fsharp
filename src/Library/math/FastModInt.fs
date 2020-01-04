namespace Compro.Math

[<RequireQualifiedAccess>]
module MInt =

    let inline init (x: ^a) (modulo: int64): int64 =
        let x = (int64 x) % modulo
        match x with
        | _ when x < 0L -> x + modulo
        | _ when x >= modulo -> x - modulo
        | _ -> x

    let inline add (lhs: ^a) (rhs: ^b) (modulo: int64) =
        let l, r = int64 lhs, int64 rhs
        let x = l + r

        let x =
            match x with
            | _ when x >= modulo -> x - modulo
            | _ -> x
        init x modulo

    let inline sub (lhs: ^a) (rhs: ^b) (modulo: int64) =
        let l, r = int64 lhs, int64 rhs
        let x = l - r

        let x =
            match x with
            | _ when x < 0L -> x + modulo
            | _ -> x
        init x modulo

    let inline mul (lhs: ^a) (rhs: ^b) (modulo: int64) =
        let l = (int64 lhs) % modulo
        let r = (int64 rhs) % modulo
        init (l * r) modulo

    let inline inverse (a: ^a) (modulo: int64): int64 =
        let mutable (a, b, u, v) = (int64 a, modulo, 1L, 0L)
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
        init u modulo

    let inline div (lhs: ^a) (rhs: ^b) (modulo: int64) =
        let l = (int64 lhs) % modulo
        let r = inverse rhs modulo
        init (l * r) modulo

    let inline pow (a: ^a) (n: ^b) (modulo: int64) =
        let mutable (res, a, n) = (1L, int64 a, int64 n)
        while n > 0L do
            if (n &&& 1L) = 1L then res <- res * a % modulo
            a <- a * a % modulo
            n <- n >>> 1
        res

type ModInt(modulo: int64) =
    member __.Modulo = modulo

    member __.Add(l: int64, r: int64) =
        let m = __.Modulo
        MInt.add l r m

    member __.Sub(l: int64, r: int64) =
        let m = __.Modulo
        MInt.sub l r m

    member __.Mul(l: int64, r: int64) =
        let m = __.Modulo
        MInt.mul l r m

    member __.Div(l: int64, r: int64) =
        let m = __.Modulo
        MInt.div l r m

type BiCoef =
    { modulo: int64
      fact: int64 array
      inv: int64 array
      finv: int64 array }

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module BiCoef =

    let inline init (n: ^a) (modulo: ^b): BiCoef =
        let n = int n
        let fact = Array.init n (fun _ -> 1L)
        let inv = Array.init n (fun _ -> 1L)
        let finv = Array.init n (fun _ -> 1L)
        let m, m32 = int64 modulo, int32 modulo
        for i in 2 .. n - 1 do
            let a = MInt.mul fact.[i - 1] i m
            fact.[i] <- a
            let a = MInt.mul (-inv.[m32 % i]) (m32 / i) m
            inv.[i] <- a
            let a = MInt.mul finv.[i - 1] inv.[i] m
            finv.[i] <- a

        { BiCoef.modulo = m
          fact = fact
          inv = inv
          finv = finv }

    let inline com (n: ^a) (k: ^b) (bicoef: BiCoef) =
        let n, k = int n, int k
        let m = bicoef.modulo
        match n, k with
        | _, _ when n < k -> 0L
        | _, _ when n < 0 -> 0L
        | _, _ when k < 0 -> 0L
        | _ ->
            let numer, b, c = bicoef.fact.[n], bicoef.finv.[k], bicoef.finv.[n - k]
            let denom = MInt.mul b c m
            let res = MInt.mul numer denom m
            res
