#runners

Some monads support so-called runners, functions that provide unwrapped results. In the case of `Writer`, we have the `runWriter` function, which returns a pair (result,log) as follows:
```haskell
> runWriter (sumN 5)
(15,"5,4,3,2,1,finish")
```

