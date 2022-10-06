# #catesian-product

In mathematics, specifically set theory, the Cartesian product of two sets A and B, denoted A × B, is the set of all ordered pairs (a, b) where a is in A and b is in B.

![[cartesian-product2.png|500]]

      
  -- source: [Cartesian Product](https://en.wikipedia.org/wiki/Cartesian_product), Wikipedia

## List comprehensions product Cartesian Products

```haskell
> aa = ['x','y','z']
> bb = [1,2,3]
> [(a,b)| a <- aa, b <- bb]
[('x',1),('x',2),('x',3),('y',1),('y',2),('y',3),('z',1),('z',2),('z',3)]
```

