I'm not fully understanding this error message:

```haskell
> maybeInc = (+) <$> Just 1
> :t maybeInc
maybeInc :: Num a => Maybe (a -> a)
```

So `maybeInc` is a partially applied function in the context of Maybe. The book says to use `<*>` with the function to apply it to the next argument but I'm interested in understanding the error message that you get if you don't use `<*>`.

```haskell
> maybeInc (Just 1)
```
You get the following error:
```haskell
• Couldn't match expected type: Maybe a1 -> t
              with actual type: Maybe (a0 -> a0)
• The function ‘maybeInc’ is applied to one value argument,
    but its type ‘Maybe (a0 -> a0)’ has none
  In the expression: maybeInc (Just 1)
```

The second bullet is perfectly understandable but the first with expected and actual I don't get.

I want to read it as, "`maybeInc` expected `Maybe a1 -> t` but you tried to apply it to `Maybe (a0 -> a0`".
So 
- "expected" is the type the argument the function expected to be applied to, and
- "actual" is the type of the argument you **actually** tried to apply it to.

---

Back-up a bit - what does "actual" and "expected" mean. I think:
- "Actual" is the type of the argument you tried to apply the function to.
- "Expected" is the type of the argument the function expected you to apply it to.

And to confirm that
```haskell
plusOne :: Integer -> Integer
plusOne = (+1)
```

```haskell
> plusOne 1
2

> plusOne "Hi"
<interactive>:16:9: error:
    • Couldn't match type ‘[Char]’ with ‘Integer’
      Expected: Integer
        Actual: String
    • In the first argument of ‘plusOne’, namely ‘"hi"’
      In the expression: plusOne "hi"
      In an equation for ‘it’: it = plusOne "hi"
```

So I seem to have expected and actual correctly understood. But the following error message appears to have that backward. 
```haskell
> maybeInc = (+) <$> Just 1
> :t maybeInc
maybeInc :: Num a => Maybe (a -> a)
```

- The type of `maybeInc` is a function that takes one argument in the context of a Maybe
- `maybeInc` doesn't take any arguments but you can use `<*>` to lift the argument into context and get a result
```haskell
> maybeInc <*> (Just 5)
Just 6
```

But say I don't use `<*>` and try `maybeInc (Just 1)` (which I shouldn't)
```haskell
> maybeInc (Just 1)

• Couldn't match expected type: Maybe a1 -> t
              with actual type: Maybe (a0 -> a0)
• The function ‘maybeInc’ is applied to one value argument,
    but its type ‘Maybe (a0 -> a0)’ has none
  In the expression: maybeInc (Just 1)
```

This seems to have "actual" wrong because the expected type is `Maybe a1`

This says it "expected" 

>[!DANGER] OK Carl, this isn't worth you time - STOP



