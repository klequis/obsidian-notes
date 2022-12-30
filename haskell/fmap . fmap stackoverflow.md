
https://stackoverflow.com/questions/23030638/how-fmap-fmap-typechecks

```haskell
fmap :: (x -> y) -> (g x -> g y)
fmap :: (u -> v) -> (f u -> f v)
```

- `u = g x` 
- `v = g y`


```haskell
fmap :: (  x ->   y) -> (   g x  ->    g y )
fmap :: (g x -> g y) -> (f (g x) -> f (g y))
```