# #applicative

- The `Applicative` type class extends the power of `Functor` by allowing the use of functions that are themselves in context.
- To say the same thing: The Applicative type class allows you to use functions that are inside a context, such as Maybe or IO. 
- To say the same thing again: `Applicative` generalizes `Functor`’s `fmap` to work with multiple arguments.
- That in turn allows you to chain together long sequences of computations such as `IO` or `Maybe`.

**The limitation of `Functor`**
- The limitation of `Functor` is that it only works with one argument functions. (and remember, all functions in Haskell take only one argument)
- Or a different way of saying that: The real limitation of `Functor`’s `<$>` is that if you end up with a function in a context, through partial application, you have no way of using that function.

## Class

`class Functor f => Applicative f where`

## Method `<*>` "app"

`(<*>) :: f (a -> b) -> f a -> f b`

"app takes a function inside a functor and a value inside a function and returns a transformed value inside of the same type of functor."

## Method: `pure`

`pure :: a -> f a`

"pure lifts a value into a context."

Lift a number into a `Maybe Int` returns a `Just`
```haskell
> pure 6 :: Maybe Int
Just 6


```

---

## How does `<*>` differ from `<$>`?

```haskell
(<$>) :: Functor     f ::   (a -> b) -> f a -> f b
(<*>) :: Applicative f :: f (a -> b) -> f a -> f b
```
As you can see, there is one difference which is `<*>` has the fist parameter, i.e., the function `(a -> b)` in a context where `<$>` does not.

## What does the type signature say to me?

```haskell
`(<*>) :: Applicative f => f (a -> b) -> f a -> f b`
```

- `f` is an `Applicative`
- The first parameter is a function within an `Applicative` type (e.g., `Maybe (Int -> Int)`).
- The second parameter is a value within an `Applicative`.
* The function returns a value transformed by the function in the first parameter within an `Applicative` of the same type.

## Compare `<*>` to `<$>`

Consider ...
```haskell
GHCi> (+1) <$> Just 2
Just 3
```
... works just fine. But ...
```haskell
GHCi> (+) <$> Just 2
```
... gives the error ...
```haskell
...
(maybe you haven't applied a function to enough arguments?)
...
```
... and you haven't.

If you have a function that takes two arguments (`+` is binary) and you give it one ...
```haskell
GHCi> maybeInc = (+) <$> Just 1
GHCi> :t maybeInc
GHCi> maybeInc :: Num a => Maybe (a -> a)
```
you get a partially applied function. **But**, the function is inside a `Maybe` :(. So how do you use it?

`Applicative` will allow you to make use of the function inside a `Maybe`.
```haskell
GHCi> (+) <$> Just 1 <*> Just 2
Just 3
```

**Point 1:** Using `Functor` with a function that takes 2 arguments fails.
**Point 2:** `Applicative` solves that.

`Applicative` isn't just limited to 2 arguments.

```haskell
GHCi> add3 a b c = a + b + c
GHCi> add3 <$> Just 1 <*> Just 2 <*> Just 3
Just 6
```

## Implementation of Applicative (Experimental)

>[!DANGER] Failed experiment
> I have been looking at this for days and still don't get all of it. Time to move on once I make notes on what I learned from the experiment.
> 

### What I learned
```haskell
> fmap (+) (Just 1) <*> Just 2
Just 3
> (+) <$> Just 1 <*> Just 2
Just 3
```

With respect to the above equivalent functions:

The left side of `<*>` will evaluate first to a `Maybe (Integer -> Integer)`. In other words, a function in the context of a `Maybe`.

To use that function with `Just 2` you need `<*>`.

I should always look to see if a type uses the default definition for class methods. In this case I finally realized that there were `Functor Maybe` & `Applicative Maybe` instance for `Maybe`.

Finally, perhaps a trivial or insignificant point, I wondered if `(+) 2` was `(2+)` or `(+2)`. Nathan used `(-)` to figure this out as it is not associative. 
```haskell
ghci> a = (-) 2 -- (A)
ghci> a 3
-1
ghci> b = (-) 3 -- (B)
ghci> b 2
1
```

Looking to do $3-2=1$ :
- (A) resulted in the wrong answer
- (B) resulted in the right answer
- So it looks like `(-) 3` is `(3-)`.


### Begin

```haskell
instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just a) = Just (f a)

instance Applicative Maybe where
    pure = Just
    
    Just f  <*> m       = fmap f m
    Nothing <*> _m      = Nothing

    liftA2 f (Just x) (Just y) = Just (f x y)
    liftA2 _ _ _ = Nothing

    Just _m1 *> m2      = m2
    Nothing  *> _m2     = Nothing

class Functor f => Applicative f where
  
  (<*>) :: f (a -> b) -> f a -> f b
  (<*>) = liftA2 id

  liftA2 :: (a -> b -> c) -> f a -> f b -> f c
  liftA2 f x = (<*>) (fmap f x)
```

```haskell
> fmap (+) (Just 1) <*> Just 2
Just 3
```


```haskell
> (+) <$> Just 1 <*> Just 2
Just 3
```


- `<*>` takes a function in a context and I'm starting with a function that isn't in a context so `(+) <$> Just 1` must be evaluated first. So that would be like
```haskell
b0 = (+) <$> Just 1
```
- where `b0` is a partially applied function.
- next up is `<*> Just 2`
- Is that essentially `b0 <*> Just 2`?
- Yes it is, `<*>` takes 
    - a "function in a context" and that is `b0`
    - a "value in a context" and that is `Just 2`


So let's see:
- `<*>` takes 
    1. a function in a context
    2. a value in a context
  - and returns
      1. a value in a context

---
Does `(+) 2` result in `(+2)` or `(2+)`

I was thinking about `(+) 2`. Does that result in either `(+2)` or `(2+)` or is that just a bad question?


`+` is left associative so (order of evaluation)
```haskell
1 + 2 + 3
is
(1 + 2) + 3
```
but
```haskell
1 + 2
is 
(1 + 2)
```
In which case associativity appears to be even irrelevant than it was in the first example as there is only one possible order of evaluation.



