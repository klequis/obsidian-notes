Here are the types:
```haskell
data Time a = Time (Int,Double)
data Times a = Times [Time a]
```

I have `isSameTime` which takes two `Time` and compares the time value.
```haskell
isTimeSame :: Time a1 -> Time a2 -> Bool
isTimeSame (Time t1) (Time t2) = fst t1 == fst t2
```

It might be more useful if it returned `GT`, `LT` or `EQ`. This seems to work:
```haskell
compareDates :: Time a1 -> Time a2 -> Ordering
compareDates (Time t1) (Time t2)
    | date1 > date2 = GT
    | date1 < date2 = LT
    | otherwise = EQ
  where date1 = fst t1
        date2 = fst t2
```

