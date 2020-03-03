module Compro.Test.Algorithm.BitSet

open Expecto
open Compro.Algorithm.BitSet

// https://qiita.com/drken/items/7c6ff2aa4d8fce1c9361
module MrDrken =
    [<Tests>]
    let ``bitsetを用いた出力`` =
        test "toBin" {
            let a = 0x2dUL
            let b = 0x19UL
            let expect = a &&& b
            let bs = BitSet.init 8 (a &&& b)
            Expect.equal (bs.value) expect "hoge"
            let expect = "00001001"
            Expect.equal (bs.ToBin()) expect expect
        }

    let flag0 = 1UL <<< 0
    let flag1 = 1UL <<< 1
    let flag2 = 1UL <<< 2
    let flag3 = 1UL <<< 3
    let flag4 = 1UL <<< 4
    let flag5 = 1UL <<< 5
    let flag6 = 1UL <<< 6
    let flag7 = 1UL <<< 7

    [<Tests>]
    let Item =
        test "Item" {
            let bit = flag1 ||| flag3 ||| flag5
            let bs = BitSet.init 8 bit
            let expect = "00101010"
            let actual = bs.ToBin()
            Expect.equal actual expect expect
            Expect.isTrue bs.[1] "access to 1-bit"
            Expect.isTrue bs.[3] "access to 3-bit"
            Expect.isTrue bs.[5] "access to 5-bit"
            Expect.isFalse bs.[0] "access to 0-bit"
        }

    [<Tests>]
    let ``bit-and, bit-or`` =
        test "bit-and, bit-or" {
            let bit = flag1 ||| flag3 ||| flag5
            let bs = BitSet.init 8 bit
            let bit2 = flag0 ||| flag3 ||| flag4
            let bs2 = BitSet.init 8 bit2
            let expect = bit &&& bit2
            let actual = bs &&& bs2
            Expect.equal actual.value expect "bit-and"
            let expect = bit ||| bit2
            let actual = bs ||| bs2
            Expect.equal actual.value expect "bit-or"
        }

    [<Tests>]
    let ``set`` =
        test "set" {
            let bit = flag1 ||| flag3 ||| flag5
            let bs = BitSet.init 8 bit
            let expect = bit ||| flag6
            let actual = bs.Set(6, 1)
            Expect.equal actual.value expect "bit-6 on"
            let bit = flag0 ||| flag3 ||| flag4
            let bs = BitSet.init 8 bit
            let expect = bit &&& ~~~flag3
            let actual = bs.Set(3, 0)
            Expect.equal actual.value expect "bit-3 off"
        }
