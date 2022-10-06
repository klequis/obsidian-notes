> **What is the `_` holding a place for? The function?**

## Ruben

Good question. Are you clear on what `_` is holding a place for in the following?

```
foo :: Bool -> Int
foo _ = 5
```

(Answer, it’s a wildcard for the input variable, which is of type `Bool`. The presence of `_` makes clear that the input is never used.

## Me

```
foo :: Bool -> Int
foo _ = 5
```

```haskell
> foo "hello"
• Couldn't match type ‘[Char]’ with ‘Bool’

> foo True
5
```

So the `_` is for a variable of type `Bool`, which when received will be ignored.

Back to `mempty _`.

```haskell
instance Monoid b => Monoid (a -> b) where
  mempty _ = mempty
```

```haskell
-- An a to a function
plusOne = (+1)
-- An a to b function
numToString :: (Num a, Show a) => a -> String
numToString x = show x
```

And both would be handled by `Monoid b => Monoid (a -> b)`.
`mempty (a -> b)`



What instance handles `mempty 5`
- `Monoid (Identity a)`
- `Monoid a => Monoid (a)`


Other interesting instances
- `Monoid a => Monoid (Const a b)` [source](https://hackage.haskell.org/package/base-4.17.0.0/docs/src/Data.Functor.Const.html#line-52)
- `(Monoid a, Monoid b, Monoid c) => Monoid (a, b, c)` [source](https://hackage.haskell.org/package/base-4.17.0.0/docs/src/GHC.Base.html#line-388)
- `(Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (a, b, c, d)` [source](https://hackage.haskell.org/package/base-4.17.0.0/docs/src/GHC.Base.html#line-398)
- ` Monoid (f (g a)) => Monoid (Compose f g a)` [source](https://hackage.haskell.org/package/base-4.17.0.0/docs/src/Data.Functor.Compose.html#line-47)
- 


That is clear now -- thanks.

---

If there is no monoid for an operation than a default would be meaningless (or the default for the non-monoids - which makes no sense).