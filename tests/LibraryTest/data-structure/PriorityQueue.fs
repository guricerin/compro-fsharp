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

    let que = PriorityQueue<Edge>(fun x y -> x.cost.CompareTo(y.cost))

    let a =
        { from = 0
          toward = 1
          cost = 10 }

    let b =
        { from = 4
          toward = 2
          cost = 3 }

    let c =
        { from = 2
          toward = 3
          cost = 1 }

    let d =
        { from = 1
          toward = 4
          cost = 7 }

    let e =
        { from = 9
          toward = 5
          cost = 9 }

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
