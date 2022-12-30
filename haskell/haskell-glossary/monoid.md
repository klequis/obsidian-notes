#monoid 

```haskell
class Semigroup a => Monoid a where

mempty :: a -- required, identity element
mappend :: a -> a -> a -- is `<>` from Semigroup
                       -- Will be removed from GHC in future
mconcat :: [a] -> a
```

Should satisfy these laws:
```haskell
-- Right identity
x <> mempty = x
-- Left identity
mempty <> x = x
-- Associativity
x <> (y <> z) = (x <> y) <> z (Semigroup law)
-- Concatenation
mconcat = foldr (<>) mempty
```



A monoid is a binary associative operation with an identity.

> Remember: In Haskell, an operation is a function

- A monoid is an operation.
- The operation is both binary and associative.
- It also has an identity.

It is a Semigroup with an identity.

It lets you join things together in accordance with the laws of associativity + an identity.

Some monoidal operations are:
****- summation 
- multiplication 
- list concatenation

Monoid is a type class, therefore it ( #type-class:  "give us a way to recognize, organize, and use common functionalities and patterns across types that differ in some ways but also have things in common." )

## Examples

```haskell
> mappend [1,2,3] [4,5,6]
[1,2,3,4,5,6]

> [1,2,3] <> [4,5,6] -- mappend is <>
[1,2,3,4,5,6]

> mconcat [[1..3], [4..6]]
[1,2,3,4,5,6]
```

