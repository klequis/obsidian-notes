
Write `fmap` for the below type

```haskell
fmap :: (a -> b) -> (Int -> a) -> (Int -> b)
fmap fn1 fn2 = fn1 . fn2
```

Waiting for response from Ruben