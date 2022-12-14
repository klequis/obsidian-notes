# enumFromEvery

- **[Code](https://github.com/klequis/hid-links/blob/main/ch02/radar-antenna/radarOnly/enumFromEvery.hs)**
- HID p.25

---

>One day after writing the [[#The Older Stuff (below)]] I got a new perspective on this. My previous confusion seems very silly to me now.

 If you look at the function `every`

```haskell
every :: (Enum a, Bounded a) => [a]
every = enumFrom minBound
```

It is nothing more than `enumFrom minBound`, or the `minBound` of some type.

In the code below type inference figures out the answer to "`every` what?", concludes it is `Turn` and uses that type with `every`.

You can write this:

```haskell
> enumFrom minBound :: [Turn]
[TNone,TLeft,TRight,TAround]
```

`(A)` You can use:

```haskell
aa :: [Direction]
aa = map id every
```

`(B)`Which is the same as:

```haskell
bb :: [Direction]
bb = id every
```

`(C)`And the same as:

```haskell
cc :: [Direction]
cc = every
```

But none of the above `(A)`, `(B)` or `(C)`  examples will work without a type signature.

```haskell
ff = map id every
-- "Ambiguous type variable"
```

--- 

## The Older Stuff

> This may still be interesting but see above for clarification/correctness.

I found this code very interesting
```haskell
class (Eq a, Enum a, Bounded a) => CyclicEnum a where
  cpred :: a -> a
  cpred d
    | d == minBound = maxBound
    | otherwise = pred d

  csucc :: a -> a
  csucc d
    | d == maxBound = minBound
    | otherwise = succ d

data Direction = North | East | South | West
  deriving (Eq, Enum, Bounded, CyclicEnum, Show)

data Turn = TNone | TLeft | TRight | TAround
  deriving (Eq, Enum, Bounded, Show)

rotate :: Turn -> Direction -> Direction
rotate TNone = id
rotate TLeft = cpred
rotate TRight = csucc
rotate TAround = cpred . cpred

every :: (Enum a, Bounded a) => [a]
every = enumFrom minBound

orient :: Direction -> Direction -> Turn
orient d1 d2 = head $ filter (\t -> rotate t d1 == d2) every
```

`every` and its use in `orient` made no sense to me and I'm not sure I fully get it yet.

The book explained that
```haskell
orient d1 d2 = head $ filter (\t -> rotate t d1 == d2) every
```

Was equivalent to
```haskell
orient1 d1 d2 = head $ filter (\t -> rotate t d1 == d2) [TNone, TLeft, TRight, TAround]
```

But how did `... every` get to be `[TNone, TLeft, TRight, TAround]`?

**Lets play with it a bit**

**(A1)** For type `Turn`, the `minBound` is `TNone` and the `maxBound` is `TAround`.
```haskell
> minBound :: Turn
TNone

> maxBound :: Turn
TAround
```

**(A2)** `enumFrom` takes an `a` that is an `Enum` and returns `[a]`
```haskell
> :t enumFrom
enumFrom :: Enum a => a -> [a]
```

**(A3)** If `enumFrom` is given `TNone` it will enumerate the type from `TNone` to its `maxBound` which is `TAround`
```haskell
> enumFrom TNone
[TNone,TLeft,TRight,TAround]
```

**(A4)** This is the same as **(A3)** because as noted in **(A1)** `minBound :: Turn` is `TNone`
```haskell
> enumFrom (minBound :: Turn)
[TNone,TLeft,TRight,TAround]
```

**(A5)** If you enumerate from `TLeft`, which is the second constructor in `Turn` you'll get an enumeration starting with it rather than `TNone`.
```haskell
> enumFrom TLeft
[TLeft,TRight,TAround]
```

## Summary

The type of `enumFrom` says what it does (sort of).
```haskell
enumFrom :: Enum a => a -> [a]
```

And **A1** to **A5** above show how it works.

But the implementation is a mystery to me