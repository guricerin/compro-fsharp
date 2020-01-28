namespace Compro.DataStructure.LazySegTreeClass

type LazySegTree<'Monoid, 'OpMonoid when 'OpMonoid: equality>(n: int, f: F<'Monoid>, g: G<'Monoid, 'OpMonoid>, h: H<'OpMonoid>, mUnity: 'Monoid, omUnity: 'OpMonoid) =

    let rec sizeAndHeight sAcc hAcc =
        if sAcc < n then sizeAndHeight (sAcc <<< 1) (hAcc + 1) else (sAcc, hAcc)
    // 完全二分木にしたいので2の冪数
    let _size, _height = sizeAndHeight 1 0
    /// モノイドの単位元
    let _mUnity = mUnity
    /// 作用素の単位元
    let _omUnity = omUnity
    /// 要素と要素をマージする写像
    let _f = f
    /// 要素に作用素を作用させる写像
    let _g = g
    /// 作用素と作用素をマージする写像
    let _h = h
    /// 要素
    let _nodes = Array.init (_size * 2) (fun _ -> _mUnity)
    /// 作用素
    let _lazy = Array.init (_size * 2) (fun _ -> _omUnity)

    let parent i = (i - 1) / 2
    let leftChild i = (i <<< 1) + 0
    let rightChild i = (i <<< 1) + 1
    let leafIdx k = k + _size
    /// 親から子への遅延伝播
    let propagate (k: int): unit =
        let lc, rc = leftChild k, rightChild k
        if _lazy.[k] <> _omUnity then
            if k < _size then
                _lazy.[lc] <- h _lazy.[lc] _lazy.[k]
                _lazy.[rc] <- h _lazy.[rc] _lazy.[k]
            // nodes.[k] <- apply k tree
            _nodes.[k] <- _g _nodes.[k] _lazy.[k]
            _lazy.[k] <- _omUnity

    let rec updateCore a b (x: 'OpMonoid) k l r =
        propagate k
        if r <= a || b <= l then
            _nodes.[k]
        elif a <= l && r <= b then
            _lazy.[k] <- _h _lazy.[k] x
            propagate k
            _nodes.[k]
        else
            let lc, rc, mid = leftChild k, rightChild k, (l + r) >>> 1
            let lm = updateCore a b x lc l mid
            let rm = updateCore a b x rc mid r
            _nodes.[k] <- f lm rm
            _nodes.[k]

    let rec queryCore (a: int) (b: int) (k: int) (l: int) (r: int): 'Monoid =
        propagate k
        if r <= a || b <= l then
            _mUnity
        elif a <= l && r <= b then
            _nodes.[k]
        else
            let lc, rc, mid = leftChild k, rightChild k, (l + r) >>> 1
            let lm = queryCore a b lc l mid
            let rm = queryCore a b rc mid r
            _f lm rm

    /// k番目の要素にxを代入
    member self.Set(k: int, x: 'Monoid) =
        let idx = leafIdx k
        _nodes.[idx] <- x

    /// 半開区間[l, r)を更新する
    /// O(log n)
    member self.Update(l: int, r: int, x: 'OpMonoid) = updateCore l r x 1 0 _size

    /// 半開区間[l, r)の演算結果を返す
    /// O(log n)
    member self.Query(l: int, r: int) = queryCore l r 1 0 _size

    /// k番目の要素を取得
    member self.Get(k: int) = self.Query(k, k + 1)

and F<'Monoid> = 'Monoid -> 'Monoid -> 'Monoid

and G<'Monoid, 'OpMonoid> = 'Monoid -> 'OpMonoid -> 'Monoid

and H<'OpMonoid> = 'OpMonoid -> 'OpMonoid -> 'OpMonoid
