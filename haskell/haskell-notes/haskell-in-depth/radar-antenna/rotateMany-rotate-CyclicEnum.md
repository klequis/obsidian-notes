# `rotateMany`, `rotate`, `CyclicEnum`

## rotateMany

`rotateMany` will receive 
- `Direction` such as `North` and 
- `[Turn]`'s  such as `["East","North","South","West","South","West","West"]`

It will call `rotate` which takes
- a `Turn` and
- a `Direction`

```haskell
rotateMany :: Direction -> [Turn]
rotate     :: Turn -> Direction
```

Node that the parameters are in reverse order and therefore `rotateMany` calls `rotate` using `flip`

```haskell
rotateMany foldl (flip rotate)
```

*(The use of `foldl` is interesting and will be discussed below.)*

## rotate

`rotate` receives
- `Turn` such as `TLeft` and
- `Direction` such as `North`

```haskell
rotate :: Turn -> Direction -> Direction
rotate TNone = id
rotate TLeft = cpred
rotate TRight = csucc
rotate TAround = cpred . cpred
```

So with `rotate TLeft North`

- `TLeft` is consumed by `rotate TLeft = ...`
- That leaves a partially applied function
```haskell
> :t rotate TLeft
rotate TLeft :: Direction -> Direction
```
- As a result `Direction` gets passed to `cpred`.

## CyclicEnum

- `roate` has called `cpred North`
- `cpred` is not equal to `minBound` of `Direction` so 
- `pred North` is called



A LITTLE DIVERSION FROM THIS
- minBound Turn works
- minBound Direction doesn't `(A)`

`(B)` In [file](/home/klequis/d/learn/haskell/book/haskell-in-depth/hid-links/ch02/radar-antenna/single-files/maxMinBounds.hs) if I create Turn and Direction minBound works on neither.

So why `(A)` and why `(B)`. Or in sum "What makes a type enumerable?"



















