`>>` is like `putStrLn`. It calls the next `Monoid` but does not pass anything to it

`>>=` is like
```haskell
main = do
  x = getLine
  putStrLn x
```

Would be
```haskell
main = do
  getLine >>= putStrLn
```

`return` with lambda
```haskell
\x -> return (func x)
```

- `return` will lift the result of `func x` into the `Monoid` context.
- `return` is like `pure` for `Applicative`.
```haskell
return :: a -> m a
```

> According to Get Programming With Haskell, `<-` abstracts out the creation of a lambda function.