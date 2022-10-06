

**Parametrically polymorphic** ( #parametrically-polymorphic ): Functions that cam be implemented with the same formula for any type.

**Void** ( #haskell-void ): In Haskell `Void` is a type that is not inhabited by any values (i.e: has not members).

To call a function that takes `Void` you would have to provide it with a value of type `Void`. Since there aren't any, you can never call a function that takes `Void`.

Haskellers call this function 'absurd'.

```haskell
absurd :: Void -> a
```


**Predicate** ( #predicate ): Is a two-element set. The two-element set is often called `bool`. Any function that returns `bool` (true of false) is a predicate.

**Preorder** ( #preorder ): *partial understanding* - 
1. A category where a morphism is a relation between object of less than or equal.
2. A category where there is at most one morphism going from any object a to any object b. (aka "thin" category).

**Partial order** ( #partial-order ): *not understood* - A relation where if $a \leq b$ and $b \leq a$ that $a$ is the same as $b$, is a partial order.

**Total order** ( #total-order ): Has a condition that any two object are in a relation with each other, one way or the other (but not both), resulting in a linear order.

