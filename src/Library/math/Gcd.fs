namespace Compro.Math

module Gcd =

    let inline gcd (x: ^a) (y: ^a): ^a =
        let zero = LanguagePrimitives.GenericZero

        let rec loop x y =
            if y = zero then x else loop y (x % y)
        loop x y

    let inline lcm (x: ^a) (y: ^a): ^a =
        let g = gcd x y
        x / g * y
