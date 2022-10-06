> If `mempty _ = mempty` doesn’t there need to be a definition for `mempty` on the RHS

This is what led to the question:
```haskell
instance Monoid b => Monoid (a -> b) where
  mempty _ = mempty
  mconcat = \fs x -> mconcat $ map (\f -> f x) fs
```

**Ruben:** Yes, I understand your confusion. But to emphasize: the `mempty` on the left and the `mempty` on the right are **different** functions. The one on the left belongs to the Monoid instance for `a -> b`, but the one on the right belongs to the Monoid instance for `b`.

**Me**

OK, now I see there are are two `mempty`

Constraint says `b` must be a monoid so it has to have an `mempty`.
```haskell
instance Monoid b => Monoid (a -> b) where
         --------                 -
```

And then there is the `mempty` that is being defined in the current instance
```haskell
mempty _ = mempty
------
```

How Haskell knows the `mempty` on the RHS is from the `b` feels mysterious.