module Compro.Test.DataStructure.PriorityQueue

open Expecto
open Compro.DataStructure.PriorityQueue

module Ascending =
    let que = PriorityQueue<int>(fun x y -> x.CompareTo(y))
    let ls = [ 10 .. -2 .. 2 ]

    for l in ls do
        que.Enque(l)

    [<Tests>]
    let tests =
        test "Ascending PriorityQueue" {
            Expect.equal true (que.Any()) ""
            Expect.equal 2 (que.Deque()) ""
            Expect.equal true (que.Any()) ""
            Expect.equal 4 (que.Deque()) ""
            Expect.equal true (que.Any()) ""
            Expect.equal 6 (que.Deque()) ""
            Expect.equal true (que.Any()) ""
            Expect.equal 8 (que.Deque()) ""
            Expect.equal true (que.Any()) ""
            Expect.equal 10 (que.Deque()) ""
            Expect.equal false (que.Any()) ""
        }

module Descending =
    let que = PriorityQueue<int>(fun x y -> y.CompareTo(x))
    let ls = [ 10 .. -2 .. 2 ]

    for l in ls do
        que.Enque(l)

    [<Tests>]
    let tests =
        test "Descending PriorityQueue" {
            Expect.equal 10 (que.Deque()) ""
            Expect.equal 8 (que.Deque()) ""
            Expect.equal 6 (que.Deque()) ""
            Expect.equal 4 (que.Deque()) ""
            Expect.equal 2 (que.Deque()) ""
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

    let que = PriorityQueue<Edge>(fun x y -> x.cost.CompareTo(y.cost))

    let a = init 0 1 10
    let b = init 4 2 3
    let c = init 2 3 1
    let d = init 1 4 7
    let e = init 9 5 9
    let ls = [ a; b; c; d; e ]

    for l in ls do
        que.Enque(l)

    [<Tests>]
    let tests =
        test "Dijkstra" {
            Expect.equal c (que.Deque()) ""
            Expect.equal b (que.Deque()) ""
            Expect.equal d (que.Deque()) ""
            Expect.equal e (que.Deque()) ""
            Expect.equal a (que.Deque()) ""
        }
