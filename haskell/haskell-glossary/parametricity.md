# #parametricity, #parametric

Parametricity is the property we get from having parametric polymorphism. Parametricity means that the behavior of a function with respect to the types of its (parametrically polymorphic) arguments is uniform. The behavior cannot change just because it was applied to an argument of a different type.

The type `a -> a -> a` has only two possible variations. It can return the first `a` or the second `a`. 
```haskell
f1 :: a -> a -> a
f1 x y = x 
-- or
f2 :: a -> a -> a
f2 x y = y
-- You cannot return a value that isn't `x` or `y`.
```