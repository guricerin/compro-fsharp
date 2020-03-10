module Compro.Test.DataStructure.VecDeque

open System
open Expecto
open Compro.DataStructure.VecDeque

module Base =

    [<Tests>]
    let ``Any()`` =
        test "Any()" {
            let que = VecDeque.init()
            Expect.isFalse (que.Any()) ""
            que.PushBack(1)
            Expect.isTrue (que.Any()) ""
            que.PopFront() |> ignore
            Expect.isFalse (que.Any()) ""
            Expect.throws (fun _ -> que.PopBack() |> ignore) "deque is empty"
            Expect.throws (fun _ -> que.PopFront() |> ignore) "deque is empty"
        }

module Random =
    let config = { FsCheckConfig.defaultConfig with maxTest = 100 }

    [<Tests>]
    let ``PushBack-PopFront`` =
        testProperty "push-back" <| fun (xs: int array) ->
            let q = VecDeque.init()
            for x in xs do
                q.PushBack(x)
            for i in 0 .. Array.length xs - 1 do
                let x = q.PopFront()
                Expect.equal xs.[i] x ""

    [<Tests>]
    let ``PushFront-PopBack`` =
        testProperty "push-front" <| fun (xs: int array) ->
            let q = VecDeque.init()
            for x in xs do
                q.PushFront(x)
            for i in 0 .. Array.length xs - 1 do
                let x = q.PopBack()
                Expect.equal xs.[i] x ""

    [<Tests>]
    let ``ofSeq`` =
        testProperty "ofSeq" <| fun (xs: int array) ->
            if Array.isEmpty xs then
                ()
            else
                let q: VecDeque<int> = VecDeque.build xs
                let expect = String.Join("", xs)
                let actual = String.Join("", VecDeque.ofSeq q)
                Expect.equal expect actual ""
