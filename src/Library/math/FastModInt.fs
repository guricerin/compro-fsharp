namespace Compro.Math

[<RequireQualifiedAccess>]
module ModIntInner =

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
    member __.modulo = modulo

    member __.Add(l: int64, r: int64) =
        let m = __.modulo
        ModIntInner.add l r m

    member __.Add(l: int, r: int) = __.Add(int64 l, int64 r)
    member __.Add(l: int64, r: int) = __.Add(l, int64 r)
    member __.Add(l: int, r: int64) = __.Add(int64 l, r)

    member __.Sub(l: int64, r: int64) =
        let m = __.modulo
        ModIntInner.sub l r m

    member __.Sub(l: int, r: int) = __.Sub(int64 l, int64 r)
    member __.Sub(l: int64, r: int) = __.Sub(l, int64 r)
    member __.Sub(l: int, r: int64) = __.Sub(int64 l, r)

    member __.Mul(l: int64, r: int64) =
        let m = __.modulo
        ModIntInner.mul l r m

    member __.Mul(l: int, r: int) = __.Mul(int64 l, int64 r)
    member __.Mul(l: int64, r: int) = __.Mul(l, int64 r)
    member __.Mul(l: int, r: int64) = __.Mul(int64 l, r)

    member __.Div(l: int64, r: int64) =
        let m = __.modulo
        ModIntInner.div l r m

    member __.Div(l: int, r: int) = __.Div(int64 l, int64 r)
    member __.Div(l: int64, r: int) = __.Div(l, int64 r)
    member __.Div(l: int, r: int64) = __.Div(int64 l, r)
