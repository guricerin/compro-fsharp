namespace Compro.DataStructure.BinaryHeap

open System

/// 二分ヒープ
/// 親は必ず子よりも値が 小さいor大きい
/// 兄弟同士の関係は問わない
/// 参照: https://ufcpp.net/study/algorithm/col_heap.html
/// 例：int型の値を突っ込む場合、
/// let less (x: int) (y: int) = x.CompareTo(y)
/// let greater (x: int) (y: int) = y.CompareTo(x)
/// 昇順にしたいなら less を、降順にしたいならgreter をコンストラクタに渡す
/// WARNING: IComparerじゃない関数を指定できてしまう。関数オブジェクトとかいう紛い物じゃないのがほしい...

/// BEGIN CUT HERE
type BinaryHeap<'T>(compare: 'T -> 'T -> int) =
    let _heap = ResizeArray<'T>() // 二分ヒープ
    let _compare = compare // 比較関数
    let parent n = (n - 1) / 2
    let leftChild n = (n <<< 1) + 1
    let rightChild n = (n <<< 1) + 2

    let swap x y =
        let tmp = _heap.[x]
        _heap.[x] <- _heap.[y]
        _heap.[y] <- tmp

    /// ここでの比較は昇順ソートを基準に考えている
    let compare x y = (_compare _heap.[x] _heap.[y]) < 0

    /// O(log n)
    member self.Push(x: 'T) =
        let size = _heap.Count
        _heap.Add(x)
        // 親と値を入れ替えていく
        let rec loop k =
            match k > 0 with
            | true ->
                let p = parent k
                match compare k p with
                | true ->
                    swap k p
                    loop p
                | _ -> ()
            | _ -> ()
        loop size

    /// O(log n)
    member self.Pop() =
        let res = _heap.[0]
        // 末尾ノードを根に持ってくる
        let size = _heap.Count - 1
        _heap.[0] <- _heap.[size]
        _heap.RemoveAt(size)
        // 葉ノードに達するまで子と値を入れ替えていく
        let rec loop k =
            let left = leftChild k
            match left < size with
            | true ->
                let right = rightChild k

                let c =
                    if right < size && compare right left then right else left
                match compare c k with
                | true ->
                    swap c k
                    loop c
                | _ -> ()
            | _ -> ()
        loop 0
        res

    member self.Any(): bool = _heap.Count > 0

    member self.Peek(): 'T = _heap.[0]

    member self.Dump() = String.Join(" ", _heap)
