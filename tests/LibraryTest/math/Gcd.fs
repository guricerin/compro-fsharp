namespace Compro.Test.Math

open Expecto

module Gcd =
    open Compro.Math.Gcd

    [<Tests>]
    let gcdTests =
        testList "Gcd"
            [ test "gcd int32" {
                  Expect.equal (gcd 0 0) 0 "0"
                  Expect.equal (gcd 1 0) 1 "1"
                  Expect.equal (gcd 0 1) 1 "1"
                  Expect.equal (gcd 2 1) 1 "1"
                  Expect.equal (gcd 3 1) 1 "1"
                  Expect.equal (gcd 4 6) 2 "2"
                  Expect.equal (gcd 8 12) 4 "4"
              }

              test "gcd int64" {
                  Expect.equal (gcd 0L 0L) 0L "0L"
                  Expect.equal (gcd 1L 0L) 1L "1L"
                  Expect.equal (gcd 0L 1L) 1L "1L"
                  Expect.equal (gcd 2L 1L) 1L "1L"
                  Expect.equal (gcd 3L 1L) 1L "1L"
                  Expect.equal (gcd 4L 6L) 2L "2L"
                  Expect.equal (gcd 8L 12L) 4L "4L"
              } ]

    [<Tests>]
    let lcmTests =
        testList "Lcm"
            [ test "lcm int32" {
                  Expect.equal (lcm 0 1) 0 "1"
                  Expect.equal (lcm 1 1) 1 "1"
                  Expect.equal (lcm 2 4) 4 "4"
                  Expect.equal (lcm 8 12) 24 "24"
              }

              test "lcm int64" {
                  Expect.equal (lcm 0L 1L) 0L "1L"
                  Expect.equal (lcm 1L 1L) 1L "1L"
                  Expect.equal (lcm 2L 4L) 4L "4L"
                  Expect.equal (lcm 8L 12L) 24L "24L"
              } ]
