# #functor
- The Functor type class allows you to perform computation inside a *container* such as `List` or a *context* such as `Maybe` and `IO`.
- `Functor` abstracts mapping over some value in a computational context *(HID p.52)*.
- It provides a generic interface for applying functions to values in a container or context.
- One way to think of the Functor type class is as **“things that can be mapped over.”**
- Provides the ability to transform a value within a container, including changing its type.
-  The point of Functor is to reify and be able to talk about cases where we want to reuse functions in the presence of more structure and be transparently oblivious to that additional structure. 

**Laws**
- Identity
- Composition: `fmap (f . g) == fmap f . fmap g`

**Required method(s)**
fmap :: (a -> b) -> f a -> f b

**Kind:** `* -> *`
**Synonym:** `<$>` (binary operator) <span style="color:red">Is it really a synonym or is it just a method?</span>

---

## Is `Map` a functor?

- Yes.
- Although it's kind is `* -> *` when `Map` is made an instance of `Functor` it is only concerned with a singly type variable, the one used for its values.

## `map` vs `fmap`?

- `fmap` can be used on structures that aren't lists.

## "Not altering the structure"

A functor doesn't modify the structure of the thing it maps over. If talking about a list, after using `fmap` the list is still a list and no items have been added or removed (but maybe modified).

## You can map over a nullary data constructor

```haskell
data WhoCares a =
    ItDoesnt
  | Matter a
  | WhatThisIsCalled
  deriving (Eq, Show)

```

In `WhoCares a` only `Matter a` can be `fmap` over. `ItDoesnt`  & `WhatThisIsCalled` are nullary and have no value inside to work with inside.

## Apply the function of a functor to the last argument in a data constructor and leave the others alone

### This is bad
```haskell
data CountingBad a =
  Heisenberg Int a
  deriving (Eq, Show)

instance Functor CountingBad where
  fmap f (Heisenberg n a) =
  Heisenberg (n+1) (f a)
```

This is bad because `fmap (f . g)  == fmap f . fmap g` will not hold. On the right-hand-side the first argument to `Heisenberg`, the `n` in `Heisenberg n a` will get incremented twice but on the left-hand-side it will be incremented only once.

### This is good
```haskell
data CountingGood a = 
  Heisenberg Int a
  deriving (Eq, Show)

instance Functor CountingGood where
  fmap f (Heisenberg n a) =
    Heisenberg (n) (f a)
```
This is good because nothing was done to the first type argument to `Heisenberg`.

```haskell
ghci> :t fmap (+1) []
fmap (+1) [] :: Num b => [b]
ghci> :t fmap replaceWithP []
fmap replaceWithP [] :: [Char]
```