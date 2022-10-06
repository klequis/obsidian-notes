# list comprehension question

>[!DANGER]
> List comprehensions are covered later in the books so I'm going to hold off on this question so I can move foreward.

Trying to understand what is really happening in this function

```haskell
te03 :: Integral t => [t] -> [t]
te03 xs = concat [ k | i <- xs, let k = (if even (i*i) then [i*i, i*i] else [i*i] )]
```
```haskell
ghci> te03 [1,2,3]
[1,4,4,9]
```


I see if I remove `concat`
```haskell
te03 :: Integral t => [t] -> [t]
te03 xs = [ k | i <- xs, let k = (if even (i*i) then [i*i, i*i] else [i*i] )]
```

I get
```haskell
ghci> t304 [1,2,3]
[[1],[4,4][9]]
```

So before the first k is accumulated what is the `k` on the left of the `|`? Is it `[]`?



