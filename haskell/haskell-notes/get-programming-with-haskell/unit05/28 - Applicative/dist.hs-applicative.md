## Chapter Goals
1. Learn to use `Applicative` for working with functions in context that take more than one argument.
2. Create data in the context of either `IO` or `Maybe`.

- The `Applicative` type class extends the power of `Functor` by allowing you to use **functions that are themselves in a context**.
- `Functor` can only work with one argument. `Applicative` can work with many.
- Like `Functor`, `Applicative` gives you a generalized way of working in different contexts (but with more than 1 argument).

>[!INFO] `<*>`
>Applicative’s `<*>` allows you to apply a function in a context.

>[!INFO] `<$>` & `<*>`
>You can use `<$>` and `<*>` to chain together any number of arguments.

## Distance between cities
**Goal:** Take two city names from the command line and find the distance between them using the Haversine calculation.

**Challenges:** Cities with longitude and latitude will be kept in a `Data.Map` which will return `Maybe` values. However, the `haversine` function doesn't work with `Maybe`. Its signature is:

```haskell
haversine :: LatLong -> LatLong -> Double
```

The desired signature is:
```haskell
haversine :: Maybe LatLong -> Maybe LatLong -> Double
```

You could write a wrapper for `haversine` but:
1. You would need to write a wrapper for any similar function.
2. You would need to write a different version for other similar context types such as `IO`.

>[!NOTE] 
>What I think the above two points mean is that 
>1. You would need to write a wrapper for other functions that are not in context as you need them to be.
>2. Using `Applicative` you can use `haversine` in both the `Maybe` & `IO` contexts with the same code.

So we have `haversine`:
```haskell
(LatLong -> LatLong -> Double)
```
And our desired/required function:
```haskell
(Maybe LatLong -> Maybe LatLong -> Maybe Double)
```
If you combine them you have:
```haskell
(LatLong -> LatLong -> Double) ->
    (Maybe LatLong -> Maybe LatLong -> Maybe Double)

```
Which can be generalized to:
```haskell
Functor f => (a -> b -> c) -> f a -> f b -> f c
```
>[!NOTE]
>I got really confused by thinking the last/generalized form was the signature of `Functor`, but it isn't. It is the signature of what needs to be done in this sample.

**Goal:** Make `Functor`'s `fmap` work with multiple arguments.

**Solution:**
- Perform partial application in the a context such as `Maybe` or `IO`.

**Problem**
- If you use `fmap` with partial application on the `haversine` function you will end-up with a function in a context and no way of making use of it.


>[!NOTE] Oh I see now! 
>- I was looking at the `<*>` source code to see how it lifts a function into context *but it doesn't!*
>- In the example, `haversine` is partially applied using `fmap` and you are left with a function in context.
> - And now the function is in context you can use `<*>` to apply the it further.

Given the note above note:
 - `fmap haversine`  with 1 argument -> `newFn` in context.
 - use `<*>` to apply the second argument to `newFn`.

Say it another way:
- If you use `fmap` on a function that takes more than one argument you'll end-up with a function in a context. 
- `fmap`'s signature: `fmap :: (a -> b) -> f a -> f b` clearly shows that it takes a function that *is not* in context and returns a value that is in a context.

Here is the example from the book:
```haskell
maybeInc = (+) <$> Just 1
```
and `fmap` again:
```haskell
fmap :: (a -> b) -> f a -> f b
--        (A)        (B)    (C)
```
So, 
- (A) `(+)` is the `(a -> b)`
- (B) `Just 1` is the `f a` 
- (C) is what you get back and it is in context.
- (C) could be a value or a function

Let's look at (A) - (C) in an example.
```haskell
maybeIncA = (+1) <$> Just 1
--           (D)
```
- In `maybeIncA` `,(+1)` is a partially applied function that takes one more argument.
Let's run it in GHCi
```haskell
Prelude> maybeIncA = (+1) <$> Just 1
Prelude> maybeIncA
Just 2
```
So we got back a value.
Now let's do something similar with a function that takes two arguments:
```haskell
maybeIncB = (+) <$> Just 1
```
- `maybeIncB` is a function that takes two arguments.
Let's run it in GHCi
```haskell
Prelude> maybeIncB = (+) <$> Just 1
Prelude> maybeIncB

<interactive>:18:1: error:
    • No instance for (Show (Integer -> Integer))
        arising from a use of ‘print’
        (maybe you haven't applied a function to enough arguments?)
    • In a stmt of an interactive GHCi command: print it
Prelude>
```
What's up with that? It is a partially applied function and GHCi doesn't know how to `show` it. Proof?
```haskell
Prelude> :t maybeIncB
maybeIncB :: Num a => Maybe (a -> a)
```
- Yep, a function  `(a -> a)` waiting for one argument.
- BUT - it is in a `Maybe` so you can't use `fmap` on it :(
- Working with a function that is within a context is a job for `Applicative`'s `<$>`.

>[!QUESTION] Why use `fmap` in this case
>- It's a question but not a very good one.
>- **Answer:** Because you have a value, such as `Just 1` that is already in a `Maybe` and you want to apply a function to it.
>- Duh

Finally, here is a solution to the problem with `(+)` demonstrated above:
```haskell
Prelude> maybeIncB = (+) <$> Just 1
Prelude> :t maybeIncB
maybeIncB :: Num a => Maybe (a -> a)
Prelude> a = maybeIncB <*> Just 2
Prelude> a
Just 3
```
Yep! We used `(a -> a)` which was inside a `Maybe` with a second argument - YEAH! 😃.

