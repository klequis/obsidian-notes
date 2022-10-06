# #concrete-type
I have been wondering since the beginning of my Haskell journey what a **concrete type** is. I think its meaning can be derived from the below two quotes from Get Programming With Programming p. 388

*"While we call True and False “data constructors,” in fact since they take no arguments, their values are already established and not being constructed in any meaningful sense."*

*"When a constructor takes an argument, then it is like a function in at least one sense—it must be applied to become a concrete type or value."*

My take from this is that a concrete type is one that is finalized. That is, it is fully formed and not subject to change.

So `Int` is a concrete type as is `Double`.

`Maybe a` is not a concrete type but `Maybe Int` is.

## Later Addition

Haskell Programming from First Principles confirms what I though above.

Here `f` is a concrete type. It is a `Bool` and is not waiting for any arguments:
```haskell
Prelude> f = not True
Prelude> :t f
f :: Bool
```

Here `f` is not a concrete type because it is waiting for an argument (i.e., `a`):
```haskell
Prelude> f x = x > 3
Prelude> :t f
f :: (Ord a, Num a) => a -> Bool
```

That's all folks :)