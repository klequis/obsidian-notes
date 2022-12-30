

Unlike `Functor`, that allows you to apply functions that are not in a context with a value(s) that are in a context, `Applicative` allows you to use a function that is in a context on values that are in a context.

This is clearly illustrated by comparing their types

```haskell
(<$>) :: Functor     f ::   (a -> b) -> f a -> f b
(<*>) :: Applicative f :: f (a -> b) -> f a -> f b
```

| Typeclass                               | Function in Context | Value in Context |
| --------------------------------------- | ------------------- | ---------------- |
| Functor | No                  | Yes              |
| Applicative                             | Yes                 | Yes              |

`Applicative` generalizes `Functor`'s `fmap` to work with multiple arguments.

```haskell
GHCi> (+1) <$> Just 2
Just 3
> (+) <$> Just 1
error: (maybe you haven't applied a function to enough arguments?)
> (+) <$> Just 1 <*> Just 2
Just 3
```


