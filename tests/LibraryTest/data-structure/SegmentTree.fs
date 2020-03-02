module Compro.Test.DataStructure.SegmentTree

open Expecto
open Compro.DataStructure.SegmentTree

module RSQ =

    let seg = SegTree.build [ 0; 0; 0; 0 ] 0 (+) (fun x y -> y)

    do seg.Update(0, 1)
       seg.Update(1, 2)
       seg.Update(2, 3)
       seg.Update(3, 4)

    [<Tests>]
    let tests =
        test "RangeSumQuery" {
            Expect.equal (seg.Fold(1, 0)) 0 "Out of range"
            Expect.equal (seg.Fold(0, 0)) 0 ""
            Expect.equal (seg.Fold(0, 1)) 1 ""
            Expect.equal (seg.Fold(0, 2)) 3 ""
            Expect.equal (seg.Fold(0, 3)) 6 ""
            Expect.equal (seg.Fold(0, 4)) 10 ""
            Expect.equal (seg.Fold(1, 3)) 5 ""
            Expect.equal (seg.Fold(2, 4)) 7 ""
        }

module RMQ =
    open System

    let seg = SegTree.build [| 10; 2; 5; 7; 11; 15; 999; 9; 3; 15; 13; 4; 7 |] Int32.MaxValue min (-)

    do seg.Update(6, 998) // 6番目から998を引く

    [<Tests>]
    let tests =
        test "RangeMinimumQuery" {
            Expect.equal (seg.Fold(5, 3)) Int32.MaxValue "Out of range"
            Expect.equal (seg.Fold(0, 1)) 10 ""
            Expect.equal (seg.Fold(3, 6)) 7 ""
            Expect.equal (seg.Fold(3, 7)) 1 ""
            Expect.equal (seg.Fold(0, 13)) 1 ""
            do seg.Update(6, -998)
            Expect.equal (seg.Fold(3, 7)) 7 ""
            Expect.equal (seg.Fold(0, 13)) 2 ""
        }
