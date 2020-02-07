module Compro.Test.Math.ModInt

open Expecto
open Compro.Math.ModInt

[<Tests>]
let modintTests =
    testList "ModInt(m = 1e9 + 7)"
        [ test "mult" {
              let a = ModInt.init 111111111
              let b = ModInt.init 123456789
              let c = ModInt.init 987654321
              let actual = a * b * c |> ModInt.value
              let expect = 769682799L
              Expect.equal expect actual ""
          }

          test "sub" {
              let a = ModInt.init 2000000020L
              let b = ModInt.init 20
              let actual = a - b |> ModInt.value
              let expect = 999999993L
              Expect.equal expect actual ""
          }

          test "div" {
              let a = ModInt.init 678813585
              let b = ModInt.init 100000
              let actual = a / b |> ModInt.value
              let expect = 123456789L
              Expect.equal expect actual ""
          }

          test "pow" {
              let a = ModInt.init 9999
              let b = 9999L
              let actual = a ** b |> ModInt.value
              let expect = 501911862L
              Expect.equal expect actual ""
          }

          test "add, mul, and div" {
              let a = ModInt.init 423343
              let b = ModInt.init 74324
              let c = ModInt.init 13231
              let d = ModInt.init 8432455
              let actual = (a * b + c) / d |> ModInt.value
              let expect = 79639022L
              Expect.equal expect actual ""
          } ]
