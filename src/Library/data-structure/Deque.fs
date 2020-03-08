namespace Compro.DataStructure.Deque

open System
open System.Collections
open System.Collections.Generic

/// BEGIN CUT HERE
/// 両端キュー（Double ended queue）
type Deque<'a> =
    { mutable front: 'a list
      mutable rBack: 'a list }

    interface IEnumerable<'a> with
        member self.GetEnumerator() =
            let dst =
                seq {
                    yield! self.front
                    yield! List.rev self.rBack
                }
            dst.GetEnumerator()

    interface IEnumerable with
        member self.GetEnumerator() = (self :> IEnumerable<'a>).GetEnumerator() :> IEnumerator

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Deque =

    /// O(1)
    let inline init() =
        { Deque.front = []
          rBack = [] }

    /// O(n)
    let inline build (src: 'a seq) =
        let src = src |> Seq.splitInto 2

        let item n =
            src
            |> Seq.tryItem n
            |> function
            | Some(a) -> List.ofSeq a
            | None -> []

        let front = item 0
        let rBack = item 1 |> List.rev
        { Deque.front = front
          rBack = rBack }

    let inline cons x deque =
        let front = x :: deque.front
        deque.front <- front

    let inline conj x deque =
        let rBack = x :: deque.rBack
        deque.rBack <- rBack

    let inline uncons deque =
        match deque.front, deque.rBack with
        | [], [] -> raise <| System.Exception("deque is empty")
        | hd :: tl, _ ->
            deque.front <- tl
            hd
        | _, xs ->
            let xs = xs |> List.rev
            deque.rBack <- List.tail xs
            List.head xs

    let inline unconj deque =
        match deque.front, deque.rBack with
        | [], [] -> raise <| System.Exception("deque is empty")
        | _, hd :: tl ->
            deque.rBack <- tl
            hd
        | xs, _ ->
            let xs = xs |> List.rev
            deque.rBack <- List.tail xs |> List.rev
            List.head xs

    let inline ofSeq deque =
        seq {
            yield! deque.front
            yield! List.rev deque.rBack
        }

type Deque<'a> with

    /// O(1)
    member self.PushFront(x: 'a) = Deque.cons x self

    /// O(1)
    member self.PushBack(x: 'a) = Deque.conj x self

    /// O(n)
    member self.PopFront() = Deque.uncons self

    /// O(n)
    member self.PopBack() = Deque.unconj self

    member self.IsEmpty =
        match self.front, self.rBack with
        | [], [] -> true
        | _ -> false

    member self.Any = not self.IsEmpty

    member self.Length = List.length self.front + List.length self.rBack

    member self.Dump() =
        let res = Deque.ofSeq self
        String.Join(" ", res)
