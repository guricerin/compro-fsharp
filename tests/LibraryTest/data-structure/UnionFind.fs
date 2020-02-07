module Compro.Test.DataStructure.UnionFind

open Expecto
open Compro.DataStructure.UnionFind

[<Tests>]
let tests =
    test "ATC001-UnionFind" {
        let uf = UnionFind.init 8
        let u = UnionFind.unite 1 2 uf
        Expect.isTrue u ""
        let u = UnionFind.unite 3 2 uf
        Expect.isTrue u ""
        let f = UnionFind.find 1 3 uf
        Expect.isTrue f ""
        let f = UnionFind.find 1 4 uf
        Expect.isFalse f ""
        let u = UnionFind.unite 2 4 uf
        Expect.isTrue u ""
        let f = UnionFind.find 4 1 uf
        Expect.isTrue f ""
        let u = UnionFind.unite 4 2 uf
        Expect.isFalse u "すでに連結している"
        let u = UnionFind.unite 0 0 uf
        Expect.isFalse u "すでに連結している"
        let f = UnionFind.find 0 0 uf
        Expect.isTrue f ""
    }
