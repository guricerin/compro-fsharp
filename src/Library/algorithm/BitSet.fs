namespace Compro.Algorithm.BitSet

open System

/// BEGIN CUT HERE
type BitSet =
    { value: uint64
      /// 下位ビット（二進文字列的には右端）から0-indexed
      width: int }

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module BitSet =

    /// ビットが立っている数
    /// 参照元：https://qiita.com/zawawahoge/items/8bbd4c2319e7f7746266
    let inline popcount (x: ^a): int =
        let x = uint64 x
        let x = x - ((x >>> 1) &&& 0x5555555555555555UL)
        let x = (x &&& 0x3333333333333333UL) + ((x >>> 2) &&& 0x3333333333333333UL)
        let x = (x + (x >>> 4)) &&& 0x0f0f0f0f0f0f0f0fUL
        let x = x + (x >>> 8)
        let x = x + (x >>> 16)
        let x = x + (x >>> 32)
        x &&& 0x0000007fUL |> int

    /// 二進表現におけるxのposビット目が立っているか
    let inline test (x: ^a) (pos: int): bool =
        let x = uint64 x
        x &&& (1UL <<< pos) <> 0UL

    /// xのposビット目をvに変更したものを返す
    let inline set (x: ^a) (pos: int) (v: int) =
        let x = uint64 x
        match v with
        | 0 -> x &&& ~~~(1UL <<< pos)
        | _ -> x ||| (1UL <<< pos)

    let inline flipAll (x: ^a) (width: int) =
        let x = ~~~(uint64 x)
        x &&& (1UL <<< width) - 1UL

    let inline init (w: int) (v: ^a) =
        let v = uint64 v
        let v = v &&& (1UL <<< w) - 1UL // 指定したビット幅を超える分は切り捨てる
        { BitSet.value = uint64 v
          width = w }

    let inline ofInt (v: int) =
        let w =
            (log (float v)) / (log 2.)
            |> int
            |> (+) 1
        init w v

    /// 頭が"0b"始まりでもok
    let inline ofBin (str: string) =
        let str, width =
            match str.StartsWith("0b") with
            | true -> str.Substring(2), str.Length - 2
            | _ -> str, str.Length

        let v = Convert.ToUInt64(str, 2)
        init width v

    let inline toBin (x: BitSet) =
        let v = int64 x.value
        let width = x.width
        let res = Convert.ToString(v, 2).PadLeft(width, '0')
        match res.Length <= width with
        | true -> res
        | false -> res.Substring(res.Length - width)

    let inline toHex (x: ^a) = sprintf "0x%x" x

    /// O(n)
    let inline toArray (x: BitSet) =
        let res = Array.zeroCreate x.width
        for i in 0 .. x.width - 1 do
            res.[i] <- test x.value i
        res |> Array.rev

type BitSet with

    static member inline (+) (x: BitSet, y: BitSet) =
        do x.SameWidth(y)
        { x with value = x.value + y.value }

    static member inline (+) (x: BitSet, y: int) = { x with value = x.value + uint64 y }

    static member inline (<<<) (x: BitSet, shift: int) =
        let v = x.value <<< shift
        let v = v &&& ((1UL <<< x.width) - 1UL)
        { x with value = v }

    static member inline (>>>) (x: BitSet, shift: int) =
        let v = x.value >>> shift
        let v = v &&& ((1UL <<< x.width) - 1UL)
        { x with value = v }

    static member inline (~~~) (x: BitSet) = x.Flip()

    static member inline (&&&) (x: BitSet, y: BitSet) =
        do x.SameWidth(y)
        let v = x.value &&& y.value
        { x with value = v }

    static member inline (|||) (x: BitSet, y: BitSet) =
        do x.SameWidth(y)
        let v = x.value ||| y.value
        { x with value = v }

    static member inline (^^^) (x: BitSet, y: BitSet) =
        do x.SameWidth(y)
        let v = x.value ^^^ y.value
        { x with value = v }

    /// index access
    member self.Item
        with get (idx) =
            do self.Check(idx)
            BitSet.test self.value idx

    member self.Popcount() = BitSet.popcount self.value

    member self.Any(): bool = self.Popcount() > 0

    member self.None(): bool = self.Popcount() = 0

    member private self.Check(pos: int) =
        let width = self.width
        if width <= pos then
            let msg = sprintf "pos:(%d) >= width:(%d)" pos width
            invalidArg "pos" msg

    member self.SameWidth(other: BitSet) =
        if self.width <> other.width then
            let msg = sprintf "x.width:(%d) != y.width:(%d)" self.width other.width
            invalidArg "pos" msg

    member self.Set(pos: int, x: int) =
        do self.Check(pos)
        let v = BitSet.set self.value pos x
        { self with value = v }

    member self.Reset() = { self with value = 0UL }

    member self.Flip() =
        let v = BitSet.flipAll self.value self.width
        { self with value = v }

    member self.Flip(pos: int) =
        do self.Check(pos)
        let bit =
            if BitSet.test self.value pos then 0 else 1
        self.Set(pos, bit)

    member self.ToHex() = BitSet.toHex self.value

    member self.ToBin() = BitSet.toBin self

    member self.ToArray() = BitSet.toArray self
