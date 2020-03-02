module Compro.Test.DataStructure.UnionFind

open Expecto
open Compro.DataStructure.UnionFind

[<Tests>]
let tests =
    test "ATC001-UnionFind" {
        let uf = UnionFind.init 8
        let u = uf.Unite(1, 2)
        Expect.isTrue u ""
        let u = uf.Unite(3, 2)
        Expect.isTrue u ""
        let f = uf.Find(1, 3)
        Expect.isTrue f ""
        let f = uf.Find(1, 4)
        Expect.isFalse f ""
        let u = uf.Unite(4, 2)
        Expect.isTrue u ""
        let f = uf.Find(1, 4)
        Expect.isTrue f ""
        let u = uf.Unite(4, 2)
        Expect.isFalse u "すでに連結している"
        let u = uf.Unite(0, 0)
        Expect.isFalse u "すでに連結している"
        let f = uf.Find(0, 0)
        Expect.isTrue f ""
    }
