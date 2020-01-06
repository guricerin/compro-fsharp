namespace Compro.DataStructure.MutablePriorityQueue

open System
open System.Collections.Generic

type PriorityQueue<'T when 'T: comparison>(comparer: IComparer<'T>, isDescending: bool) =
    let _heap = ResizeArray<'T>()
    let _comparer = comparer
    let _isDescending = isDescending

    let parent i = (i - 1) / 2
    let leftChild i = (i <<< 1) + 1
    let rightChild i = (i <<< 1) + 2

    member self.Compare(x, y) =
        match _isDescending with
        | true -> _comparer.Compare(y, x) // 降順
        | false -> _comparer.Compare(x, y) // 昇順

    member self.Any() = _heap.Count > 0

    member self.Swap(x: int, y: int) =
        let tmp = _heap.[x]
        _heap.[x] <- _heap.[y]
        _heap.[y] <- tmp

    member self.Enque(item: 'T) =
        _heap.Add(item)
        let mutable i = _heap.Count - 1

        let rec loop() =
            match i <= 0 with
            | true -> ()
            | false ->
                let p = parent i
                match self.Compare(_heap.[i], _heap.[p]) >= 0 with
                | true -> ()
                | false ->
                    self.Swap(i, p)
                    i <- p
                    loop()
        loop()

    member self.Deque() =
        let res = _heap.[0]
        let last = _heap.Count - 1
        let lastItem = _heap.[last]

        let mutable p = 0

        let rec loop() =
            let mutable l = leftChild p
            match l >= last with
            | true -> ()
            | false ->
                let r = rightChild p
                if r < last && self.Compare(_heap.[r], _heap.[l]) < 0 then l <- r
                let l = l
                match self.Compare(_heap.[l], lastItem) >= 0 with
                | true -> ()
                | false ->
                    self.Swap(p, l)
                    p <- l
                    loop()
        loop()

        _heap.[p] <- lastItem
        _heap.RemoveAt(last)
        res

    member self.Debug() = printfn "%s" (String.Join(" ", _heap))
