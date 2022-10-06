# #semigroup
```haskell
class Semigroup a where

(<>) : a -> a -> a -- append
```


Should satisfy these laws
```haskell
-- Associativity
x <> (y <> z) = (x <> y) <> z
```


**In mathematics**, a semigroup is an algebraic structure consisting of a set together with an associative #internal-binary-operation on it. (source: Wikipedia)

**Semigroup**. A _semigroup_ is a type class with a single binary operator, often called _append_ or _combine_, that is _associative_. (source: [# A Glossary of Functional Programming](https://degoes.net/articles/fp-glossary)), John A De Goes.

