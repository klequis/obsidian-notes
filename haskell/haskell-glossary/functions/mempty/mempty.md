# #mempty

- must provide a value of type `(a -> b)` -- got it.

 ```haskell
instance Monoid b => Monoid (a -> b) where
  mempty _ = mempty
  mconcat = \fs x -> mconcat $ map (\f -> f x) fs
```

I don't really get that yet :(. As I think through you post I'm getting more questions.

The monoid for `[a]` has
```haskell
mempty = []
```

It doesn't have the `_` (whatever placeholder) like monoid for `(a -> b)`.
```haskell
mempty _ = mempty
```

1. What is the `_` holding a place for (and ? The function? Perhaps like:
```haskell
> mempty (+1)
()
```

2. Since the monoid for `[a]` is `[]` why does this return `()`?
```haskell
> mempty [3]
()
```

I'm not sure I really understand "unit" `()`. There is `data ()` in hoogle so seems to be a type that gets returned to mean "returned nothing" or "resolved to nothing".

I don't really understand "unit" `()`. Does my lack of clarity on that relate to the original question?




---

`()` is "unit" and my understanding is it is like saying "didn't return anything" or "resolved to nothing".

I'm assuming it is `data ()` and separate from an empty tuple - is that correct? 


---
---
- **No default implementation** - I don't see a default implementation for `mempty`.
-  


# Understanding `mempty`.

I'm doing some exercises on writing instance of `Semigroup` & `Monoid` and finding it hard to relate to `mempty`.

It looks to me that there is no default implementation of `mempty`. Is that the case?

However, as I look at `Monoid` instances in GHC (at least the ones I can somewhat relate to) I'm often left wondering what `mempty` really is. 

I totally get this
```haskell
instance Monoid [a] where
  mempty = []
  mconcat xss = [x | xs <- xss, x <- xs]
```

The answer seems to be that `mempty` is what it is defined as in the instance. But then I saw this and was left wondering:

```haskell
instance Monoid b => Monoid (a -> b) where
  mempty _ = mempty
  mconcat = \fs x -> mconcat $ map (\f -> f x) fs
```

Trying to guess what that means ... For any function `(a -> b)` where `a` is `mempty` the function is `mempty`.






## Lots of examples from GHC
```haskell
instance Monoid [a] where
  mempty = []
  mconcat xss = [x | xs <- xss, x <- xs]
```

```haskell
instance Monoid b => Monoid (a -> b) where
  mempty _ = mempty
  mconcat = \fs x -> mconcat $ map (\f -> f x) fs
```

```haskell
instance Monoid () where
  mempty = ()
  mconcat _ = ()
```

```haskell
instance Monoid Ordering where
  mempty = EQ
```

```haskell
data Precision = Approximate | Precise 
  deriving (Eq, Show)

instance Monoid Precision where
  mempty = Precise
  mappend = (Semi.<>) -- Semi alias for Semigroup
```

```haskell
newtype Nablas = MkNablas (Bag Nabla)

instance Semigroup Nablas where
  MkNablas l <> MkNablas r = MkNablas (l `unionBags` r)

instance Monoid Nablas where
  mempty = MkNablas emptyBag
```

```haskell
data DisambigInfo
       = NoOccurrence
       | UniqueOccurrence GlobalRdrElt
       | DisambiguatedOccurrence GlobalRdrElt
       | AmbiguousOccurrence (NE.NonEmpty GlobalRdrElt)
```

```haskell
instance Semi.Semigroup DisambigInfo where
  _ <> DisambiguatedOccurrence g' = DisambiguatedOccurrence g'
  DisambiguatedOccurrence g' <> _ = DisambiguatedOccurrence g'
  NoOccurrence <> m = m
  m <> NoOccurrence = m
  UniqueOccurrence g <> UniqueOccurrence g'
            = AmbiguousOccurrence $ g NE.:| [g']
  UniqueOccurrence g <> AmbiguousOccurrence gs
            = AmbiguousOccurrence (g `NE.cons` gs)
  AmbiguousOccurrence gs <> UniqueOccurrence g'
            = AmbiguousOccurrence (g' `NE.cons` gs)
  AmbiguousOccurrence gs <> AmbiguousOccurrence gs'
            = AmbiguousOccurrence (gs Semi.<> gs')

```

```haskell
data HasGivenEqs
  = NoGivenEqs
  | LocalGivenEqs
  | MaybeGivenEqs
  deriving Eq

instance Semigroup HasGivenEqs where
  NoGivenEqs <> other = other
  other <> NoGivenEqs = other
  MaybeGivenEqs <> _other = MaybeGivenEqs
  _other <> MaybeGivenEqs = MaybeGivenEqs
  LocalGivenEqs <> LocalGivenEqs = LocalGivenEqs


instance Monoid HasGivenEqs where
  mempty = NoGivenEqs
```