@f-a
I had defined an `Eq` instance for `Time` (wrote it as below) but since it is only comparing `fst` of `Time` it seemed samantically incorrect and so wrote a function with a more accurate name.

```haskell
instance Eq (Time a) where
  (==) (Time t1) (Time t2) 
           | fst t1 == fst t2 = True
           | otherwise = False
```
 
I removed the useless ytpe parameter from `Time` and see that works but I don't know why it isn't needed where `Times a` does?

@mhitza

Very interesting how you used the alias. Your comment says "alias Times" but did you mean "alias Time"?

I get `[Time Double]` now. `a` is the parameter to `Time` in `Time a` so `tVal` becomes a `Double`. Sometimes I get thrown off because something look different so I think it is.

And I get `ptimes` as well now

---

