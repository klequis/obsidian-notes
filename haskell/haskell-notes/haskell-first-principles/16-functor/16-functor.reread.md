
1

> ` -> ` is a type constructor

> Every argument to the type constructor of ` -> ` must be of kind `*`

---
3

> The kind of `f` must be `* -> *`


---
3

> The f in the type class definition for `Functor f` must be the same `f` throughout the entire definition, and it must refer to a type that implements the type class.

`f` does not mean `Functor`. It does mean a “type that implements `Functor`".


---
4

```haskell
fmap :: (a -> b) -> f a -> f b
```
 
 > The argument `f a` is a `Functor` `f` that takes a type argument `a`.
 
 > That is, the `f` is a type that has an instance of the `Functor` type class.

---
5

```haskell
fmap :: (a -> b) -> f a -> f b
```

> **Rule:** Each argument (and result) in the type signature for a function must be a fully aplied type. Each argument must be of kind `*`

> Since f a and f b must each have the kind *, f by itself must have kind * -> *.

`f` is an instance of `Functor` and has kind `* -> *`

`f a` has kind `*`


---
6

> Each argument and result of every function must be a type constant, not a type constructor. Given that knowledge, we can know something about Functor from the type of fmap:

```haskell
class Functor f where
fmap :: (a -> b) -> f a -> f b
--       [  1 ]     [2]    [3]
-- 1, 2, and 3 all have the kind *
```

- 1, 2 and 3 all have the kind `*`
- The `f` introduced by the class definition for Functor must accept a single type argument and thus be of kind `* -> *`. I.e., The `f` in `f a` and `f b`.


---
7

```haskell
class Biffy where
	slayer :: e a b
--           [1]
          -> (a -> c)
--           [2]  [3] 
          -> (b -> d)
          -> e c d
```

[1] 
- `e` is a type.
- `e` has kind `* -> * -> *` but `e a b` has kind `*`

[2] & [3] 
- `a -> c` is kind `* -> *` 
- `a` and `c` are both kind `*`

---
?

> A little off the subject but important

![[Pasted image 20221213101636.png]]

The `v` before the `where` binds all instance of `v`. I.e., they are all the same `v`. Therefore, `v` cannot somtimes have a parameter and other times not.

You can fix this like this:
![[Pasted image 20221213102131.png]]

Or:
![[Pasted image 20221213102225.png]]

---
8

```haskell
($) :: (a -> b) -> a -> b -- infixr 0
```

**Application operator**

- This operator is redundant, since ordinary application `(f x)` means the same as `(f $ x)`. 
- However, `$` has low, right-associative binding precedence, so it sometimes allows parentheses to be omitted; for example:

```haskell
f $ g $ h x  =  f (g (h x))
```

It is also useful in higher-order situations, such as `map ($ 0) xs`, or `zipWith ($) fs xs`.

---


```haskell
data FixMePls a = FixMe | Pls a
  deriving (Eq, Show)
```

This works:
![[Pasted image 20221213104545.png]]

But this dosn't because `(FixMePls a)` is a fully applied type with kind `*` and `fmap` requires kind `* -> *`.
![[Pasted image 20221213104712.png]]
• Expected kind ‘* -> *’, but ‘FixMePls a’ has kind ‘*’

---
9

```haskell
(<$>) :: Functor f => (a -> b) -> f a -> f b
($) ::                (a -> b) ->   a ->   b
```

Functor is **a type class for function application** “over” or “through” some structure f that we want to ignore and leave untouched.


---
======================================
---

- function                              `g :: a -> b`
- function                              `f :: b -> c`
- you must have                    `h :: a -> c`
- so                                         `h(x) = f(g(x))`

- `f ∘ g` is frequently called `f` after `g`.



---

# Back to the original question

```haskell
> (fmap . fmap) sum Just [1, 2, 3] -- [B] Just 6
> :t (fmap . fmap) sum Just
(fmap . fmap) sum Just :: (Foldable t, Num b) => t b -> Maybe b
```

- sum

```haskell
> :t sum
sum :: (Foldable t, Num a) => t a -> a

> :t Just
Just :: a -> Maybe a

> :t sum Just
sum Just :: (Foldable ((->) a), Num (Maybe a)) => Maybe a
```


- `sum` takes a `t a`
- `Just` is a `a -> Maybe a`
- `sum Just`  ->  `Maybe a`


# Ruben

First `fmap`
```haskell
$dFunctor :: Functor ((->) [Integer])
_ :: (Maybe [Integer] -> Maybe Integer)
-> ([Integer] -> Maybe [Integer]) -> [Integer] -> Maybe Integer
_ :: forall (f :: * -> *) a b. Functor => (a -> b) -> f a -> f b
```

Second `fmap`
```haskell
$dFunctor :: Functor Maybe

_ :: ([Integer] -> Integer) -> Maybe [Integer] -> Maybe Integer
_ :: forall (f :: * -> *) a b. Functor f => (a -> b) -> f a -> f b
```


# Owen







```haskell
( fmap . fmap) sum  Just  [1,2,3] 
((fmap . fmap) sum) Just  [1,2,3]  --by left-associative function precedence
((fmap (fmap sum))  Just) [1,2,3]  --by expanding the . 
((fmap sum) . Just) [1,2,3]        --by how functions (such as `Just`) are 
--                                   functors
(fmap sum) (Just [1,2,3])          --by expanding the . 
Just (sum [1,2,3])                 --by the Maybe functor instance
Just 6
```










g4 = (fmap sum) (Just [1,2,3])          -- by expanding the . 
-- :t sum
-- sum :: (Foldable t, Num a) => t a -> a
-- :t (fmap sum)
-- (fmap sum) :: (Functor f, Foldable t, Num b) => f (t b) -> f b
-- :t (fmap sum) (Just)
-- (fmap sum) (Just) :: Num b => b -> b





```haskell
(fmap .  fmap sum)  Just   [1,2,3]      --ff
(fmap . (fmap sum)) Just   [1,2,3]      --function application precedes over `.`
(fmap   (fmap sum   Just)) [1,2,3]      --expanding the .
(fmap   ( sum  .  Just ))  [1,2,3]      --by by how Just is a functor
[(sum . Just) 1, (sum . Just) 2, (sum . Just) 3]   --by how the list is a functor
[sum (Just 1), sum (Just 2), sum(Just 3)]
[1,2,3]                                 --by the Foldable instance of Maybe
```


```haskell
(fmap .  fmap sum)  Just   [1,2,3]      --ff
(fmap . (fmap sum)) Just   [1,2,3]      --function application precedes over `.`
(fmap   (fmap sum   Just)) [1,2,3]      --expanding the .
(fmap   ( sum  .  Just ))  [1,2,3]      --by by how Just is a functor
[(sum . Just) 1, (sum . Just) 2, (sum . Just) 3]   --by how the list is a functor
[sum (Just 1), sum (Just 2), sum(Just 3)]
[1,2,3]                                 --by the Foldable instance of Maybe
```

---

Let me phrase my suggestion another way:

In both of Carl's `fmap . fmap ...`  puzzles, the form `fmap f Just` occurs. 

According to the type of `fmap :: (a -> b) -> f a -> f b` , this implies that `Just` is an `f b` for some `Functor f.` 

I suggest that this `Functor f `may be the **function constructor**. 

Starting point: Look at all instances of Functor. Do any of them look like related to functions? E.g. (->b) or (b->)? 


```haskell
> :t fmap Just
fmap Just :: Functor f => f a -> f (Maybe a)

> fmap Just [1,2,3]
[Just 1,Just 2,Just 3] -- [A]

-- Lining-up [A] with the type of `:t fmap Just`
fmap Just :: Functor f => f a      -> f    (Maybe a)
                          [1,2,3]     []    Just 1, Just 2, Just 3
```

Given that, what is

```haskell
fmap
```

#### Comparing

```haskell
dd :: Maybe Integer
dd = fmap sum (Just [1,2,3])
6

ee :: Maybe Integer
ee = (fmap . fmap) sum Just [1, 2, 3]
Just 6

ff :: [Integer]
ff = (fmap . fmap sum) Just [1,2,3]
[1,2,3]

-- ------------------------------------------------



```


1. fmap takes an a -> b and an f a to give you an f b
2. In this case our a -> b is Just so all we need to do is add an f a; 

what would we get if Just was show or Right or *3 instead?

essentially fma

---

? In this case, what is (fmap (fmap sum) Just) ?
```haskell
(fmap (fmap sum) Just)
```

---

```haskell
> :t fmap (fmap sum) Just
fmap (fmap    sum) Just ::   (Foldable t, Num b) => t b -> Maybe b

> :t (fmap . fmap) sum Just
(fmap . fmap) sum  Just ::   (Foldable t, Num b) => t b -> Maybe b

-- and as expected
:t fmap (fmap sum) Just [1 :: Int,2,3]
fmap (fmap sum) Just [1 :: Int,2,3] :: Maybe Int


```


---

And this is part of the answer to the question I my have not fully formulated yet.

```haskell
(fmap (fmap sum) Just) [1,2,3] -- didn't expect that would work but point well made.

```

which is the same as
```haskell
fmap (fmap sum) Just [1,2,3]
```

I'm wondering if I broke this into steps how is it being done

It is a process going from right to left







