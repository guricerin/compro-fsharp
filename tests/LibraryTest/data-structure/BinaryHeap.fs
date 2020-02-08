module Compro.Test.DataStructure.BinaryHeap

open Expecto
open Compro.DataStructure.BinaryHeap

module Ascending =
    let que = BinaryHeap<int>(fun x y -> x.CompareTo(y))
    let ls = [ 10 .. -2 .. 2 ]

    for l in ls do
        que.Push(l)

    [<Tests>]
    let tests =
        test "Ascending PriorityQueue" {
            Expect.equal true (que.Any()) ""
            Expect.equal 2 (que.Pop()) ""
            Expect.equal true (que.Any()) ""
            Expect.equal 4 (que.Pop()) ""
            Expect.equal true (que.Any()) ""
            Expect.equal 6 (que.Pop()) ""
            Expect.equal true (que.Any()) ""
            Expect.equal 8 (que.Pop()) ""
            Expect.equal true (que.Any()) ""
            Expect.equal 10 (que.Pop()) ""
            Expect.equal false (que.Any()) ""
        }

module Descending =
    let que = BinaryHeap<int>(fun x y -> y.CompareTo(x))
    let ls = [ 10 .. -2 .. 2 ]

    for l in ls do
        que.Push(l)

    [<Tests>]
    let tests =
        test "Descending PriorityQueue" {
            Expect.equal 10 (que.Pop()) ""
            Expect.equal 8 (que.Pop()) ""
            Expect.equal 6 (que.Pop()) ""
            Expect.equal 4 (que.Pop()) ""
            Expect.equal 2 (que.Pop()) ""
        }

module Edge =
    type Edge =
        { from: int
          toward: int
          cost: int }

    let init f t c =
        { from = f
          toward = t
          cost = c }

    let que = BinaryHeap<Edge>(fun x y -> x.cost.CompareTo(y.cost))

    let a = init 0 1 10
    let b = init 4 2 3
    let c = init 2 3 1
    let d = init 1 4 7
    let e = init 9 5 9
    let ls = [ a; b; c; d; e ]

    for l in ls do
        que.Push(l)

    [<Tests>]
    let tests =
        test "Dijkstra" {
            Expect.equal c (que.Pop()) ""
            Expect.equal b (que.Pop()) ""
            Expect.equal d (que.Pop()) ""
            Expect.equal e (que.Pop()) ""
            Expect.equal a (que.Pop()) ""
        }
