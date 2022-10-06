# #fromIntegral

`fromIntegral` takes an integral and forces it to implement the `Num` type class, rendering it polymorphic. 



> [!QUESTION] Question

Instead of this saying "rendering it polymorphic", would it be more accurate to say "making in more polymorphic"? Because: an `Integral` can be an `Int` so it is already polymorphic. Forcing it into `Num` makes it more so.

This shows that `a` is an `Integral` but when used in an equation with `b` which is an `Int`, it becomes an `Int`.
```haskell
> a = 5 :: Integral a => a
> :t a
a :: Integral a => a
> b = 2 :: Int
> :t b
b :: Int
> c = a + b
> :t c
c :: Int
```

> [!QUESTION]  END

