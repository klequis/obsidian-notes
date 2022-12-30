
# 1: `Functor f => f1 f2`

I need to think about `f1` & `f2` in

```haskell
[A] | (fmap . fmap)          | (Functor f1, Functor f2)                    => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
[B] | (fmap . fmap) sum      | (Functor f1, Functor f2, Foldable t, Num b) => f1 (f2 (t b)) -> f1 (f2 b)
[C] | (fmap . fmap) sum Just | (                        Foldable t, Num b) => t b -> Maybe b
```

## [A]

```haskell
[A] | (fmap . fmap)          | (Functor f1, Functor f2)                    => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
```

```haskell
(fmap . fmap)
(a -> b) -> f1 (f2 a) -> f1 (f2 b)
```

You apply the 2nd fmap to the second parameter and then you apply the 1st fmap to the result of the application of the first. Something like this:
```haskell
aa = fmap . fmap (*2) (+1)
```

```haskell
:t (fmap . fmap)               (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
:t fmap . fmap (*2) (+1)       (Functor f, Num (a -> b)) => (a -> b) -> f a -> f b

```



# 2

I should remember this syntax
```haskell
> :k (->) Int
(->) Int :: * -> *
> :k (->) Int Int
(->) Int Int :: *
```

It works kind of like `(,) 1 2`

# 3

What does
```haskell
f ~ ( (->) [Integer] )
```

mean in the context of
```haskell
a ~ Maybe [Integer]
b ~ Maybe Integer
f ~ ( (->) [Integer]
(f a) ~ ([Integer] -> Maybe [Integer])
(f b) ~ ([Integer] -> Maybe Integer)
```


---

So  `(->) [Integer]` is ??
```haskell
> :k (->)
(->) :: * -> * -> *
> :k (->) [Integer]
(->) [Integer] :: * -> *
```
And you can write it as ??
```haskell
[Integer] -> * -> *
```


So I think I'm making progress on that.


**Hopefully this is a related question**

**1)**
```haskell
h20 = sum 1
```
`Ambiguous type variable ‘t0’ arising from a use of ‘sum’  prevents the constraint ‘(Foldable t0)’ from being solved.`
Makes sense bec 1 isn't a `Foldable`.

**2)**
```haskell
h2a = sum [1]
```
Works as expected. [] has a `Foldable` instance

**3)**
```haskell
h2b = fmap sum Just 1
```
I expected this to fail because `fmap` lifts `sum` over `Just` and I'm thinking you get something like `Just (sum 1)` and since `1` isn't a `Foldable` I'm expecting the `Ambiguous type variable ...` again ??

**4)**
```haskell
h2d = sum (Just 1)
```
Works I'm thinking about it as
```haskell
sum :: (Foldable t, Num a) => t a -> a
```
- `Just` is a `Foldable`, and
- `1` is a `Num`

`h2d` makes sense to be where `h2b` doesn't?

**5)**

This is from [one of ruben's posts](https://discourse.haskell.org/t/question-on-map-map-sum/5421/3?u=klequis) where he said "...(or more manually, inserting `_` in a repl)". 
This seems to be what he meant but I don't yet understand all of the information it gives.

```haskell
ghci> _ :: Maybe Integer -> Integer

<interactive>:237:1: error:
    • Found hole: _ :: Maybe Integer -> Integer
    • In the expression: _ :: Maybe Integer -> Integer
      In an equation for ‘it’: it = _ :: Maybe Integer -> Integer
    • Relevant bindings include
        it :: Maybe Integer -> Integer (bound at <interactive>:237:1)
      Valid hole fits include
        maximum :: forall (t :: * -> *) a. (Foldable t, Ord a) => t a -> a
          with maximum @Maybe @Integer
          (imported from ‘Prelude’ at app/Main.hs:4:8-11
           (and originally defined in ‘Data.Foldable’))
        minimum :: forall (t :: * -> *) a. (Foldable t, Ord a) => t a -> a
          with minimum @Maybe @Integer
          (imported from ‘Prelude’ at app/Main.hs:4:8-11
           (and originally defined in ‘Data.Foldable’))
        product :: forall (t :: * -> *) a. (Foldable t, Num a) => t a -> a
          with product @Maybe @Integer
          (imported from ‘Prelude’ at app/Main.hs:4:8-11
           (and originally defined in ‘Data.Foldable’))
        sum :: forall (t :: * -> *) a. (Foldable t, Num a) => t a -> a
          with sum @Maybe @Integer
          (imported from ‘Prelude’ at app/Main.hs:4:8-11
           (and originally defined in ‘Data.Foldable’))

```

---

So what is up with **3)**

- `Maybe` has a `Foldable` instance
- `1` is a Num

```haskell
h2b = fmap sum Just 1

fmap sum :: (Functor f, Foldable t, Num b) => f (t b) -> f b
fmap sum Just :: Num b => b -> b

fmap :: Functor f => (a -> b) -> f a -> f b
sum :: (Foldable t, Num a) => t a -> a
```


Breaking down the types
```haskell
fmap sum      :: (Functor f, Foldable t, Num b) => f (t b) -> f b
fmap sum Just :: Num b  => b -> b
```

It is as if adding `Just`, which is both `Functor` and `Foldable`  satisfied the requirements of `(Functor f, Foldable t)` and just leaves `Num b` remaining.

---
#### See if the above way of looking at things helps with the original problem

```haskell
(fmap . fmap) sum Just [1, 2, 3]

(fmap . fmap) sum :: (Functor f1, Functor f2, Foldable t, Num b) => f1 (f2 (t b)) -> f1 (f2 b)
```

- `Functor f1` - `Just`
- `Functor f2` - `[]`
- `Foldable t` - `[]`
- `Num b` - `1,2,3`

next: [[Working out types for "functor - where am I at"]]

---

`fmap`
- `(a -> b)` is `sum`
- `f a` is `Just 1`

So I get
`sum (Just 1)  -> f b`

`sum Just 1` doesn't work bec it looks like 2 separate arguments

`sum (Just 1)` does, 1 argument, and
`sum :: (Foldable t, Num a) => t a -> a`
- `Just` is `Foldable`
- `1` is `Num`
So that works.

**So why does `fmap sum Just 1` work?**

If I look at it as `Just` being a container that is lifted over I'm back to `Just (sum 1)` which is obviously wrong.


