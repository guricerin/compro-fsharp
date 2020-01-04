namespace Compro

module Util =
    let strRev (s: string): string =
        s
        |> Seq.rev
        |> Seq.map string
        |> String.concat ""

    let inline roundup (x: ^a) (y: ^a): ^a =
        let one = LanguagePrimitives.GenericOne
        (x + y - one) / y

    let inline sameParity (x: ^a) (y: ^a): bool =
        let one = LanguagePrimitives.GenericOne
        (x &&& one) = (y &&& one)
