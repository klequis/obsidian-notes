Maybe
- Foldable
- Functor

[] List
- Foldable
- Functor


## Ex 1


```haskell
(fmap . fmap) sum Just [1, 2, 3]
Just 6

(fmap . fmap) sum :: (Functor f1, Functor f2, Foldable t, Num b) => f1 (f2 (t b)) -> f1 (f2 b)
```

- `Functor f1` - `Just`
- `Functor f2` - `[]`
- `Foldable t` - `[]`
- `Num b` - `1,2,3`

```haskell
 fmap                  ::  Functor f                                  => (a -> b) ->      f a      ->      f b
(fmap . fmap)          :: (Functor f1, Functor f2                   ) => (a -> b) -> f1 (f2 a)     -> f1 (f2 b)
(fmap . fmap) sum      :: (Functor f1, Functor f2, Foldable t, Num b) =>             f1 (f2 (t b)) -> f1 (f2 b)
(fmap . fmap) sum Just :: (                        Foldable t, Num b) =>                     t b   ->  Maybe b
```


## Ex 2

```haskell
fmap sum [Just 1]

fmap sum :: (Functor f, Foldable t, Num b) => f (t b) -> f b
```

- `Functor f` - `[]`
- `Foldable t` - `Just`
- `Num b` - `1`

## Ex 3

Is it valid to look like types like this?

If I look at

```haskell
fmap (>3) (+1)

fmap           ::  Functor f                => (a -> b) -> f a -> f      b     -- [A]
fmap (>3)      :: (Functor f, Ord a, Num a) =>             f a -> f      Bool  -- [B]
fmap (>3) (+1) :: (           Ord a, Num a) =>               a ->        Bool  -- [C]
fmap (>3) (+1) :: (           Ord a, Num a) =>                    (->) a Bool  -- [C]

fmap :: Functor f => (a -> b) -> f a -> f b -- [A] 
fmap (>3) :: (Functor f, Ord a, Num a) => f a -> f Bool -- [B] 

f Bool
-- becomes
(->) a Bool  -- In this case you instantiate `f` to `(->) a`
-- because
(+3) :: Num a => a -> a
-- was added

```

Added `(+3)`
- A) `f a` went away because that argument was supplied - OK
	- Again, sinc a function `(+3)` satisfied the requirement of a `Functor f => f a`, it appears that functions are `Functors`.
- B) `Functor f` go away - but why? 1. There are no more `f` on the right side of `=>`. The final `f` was replace with `(->) a`
- C) `f b` became `(->) a Bool`, which could be thought of as `(a -> Bool)`

`(+3)` is `Num a => a -> a`. It is waiting for one more `a`
I'll guess that is where the `a` in `(a -> Bool)` came from.



[quote="jaror, post:32, topic:5421"]
Every time you apply an argument one of the arrows disappear.
[/quote]

True but overlooked - thanks for pointing out.

[quote="jaror, post:32, topic:5421"]
In this case you instantiate `f` to `(->) a`
[/quote]



---





## Ex 4

```haskell
fmap sum [Just 1]

fmap sum          :: (Functor f, Foldable t, Num b)         => f (t b) -> f b
fmap sum []       ::                         Num b          =>            [b]
fmap sum [Just]   :: (Foldable ((->) a),     Num (Maybe a)) =>            [Maybe a]
fmap sum [Just 1] ::                         Num b          =>            [b]
```

- `(Foldable ((->) a)` a functor that is also a foldable
- `Num (Maybe a))` - A `Maybe` that must be an instance of `Num`


next: [[Question on Ex 4]]

## Ex 5

```haskell
(fmap (+1) . fmap (*2))  [1..5] -- [3,5,7,9,11]

(fmap (+1) . fmap (*2)) :: (Functor f, Num b) => f b -> f b
```

- `f` ~ `[]`
- `Num` ~ `1..5`




---

## Write about it

I think part of what had led to my confusion was the term "lifts over" and maybe the idea of types as containers. In my mind "lifts over" worked like this:

![[Pasted image 20221216104512.png]]

That is, I thought that `sum` just hopped over `[]` basically ignoring it. However, that is not what is happening

```haskell
fmap sum :: (Functor f, Foldable t, Num b) => f (t b) -> f b
```

