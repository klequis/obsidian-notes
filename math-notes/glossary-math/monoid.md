> [!DANGER] Deprecated
> See #monoid

# Monoid
---
## Monoid
*Haskell Programming from First Principles, p. 574, sec 15.3*

A *monoid* is an *operation*

> Let's you join things together
> - summation
> - multiplication
> - concatenation
> - etc

It
- is an *operation*
- is *binary*
- is *associative*
- has an *identity*
- is a *type class* (in Haskell)

- A monoid is a binary associative operation with an identity.
- In plain English, a monoid is a function that takes two arguments and follows two laws: associativity and identity.

```haskell
class Semigroup a => Monoid a where
	mempty :: a
	mappend :: a -> a -> a
	mconcat :: [a] -> a
	mconcat = foldr mappend mempty
```

> Hey, remember, this is the default. There can be other implementations.
---

## Monoid
*Prelude [Monoid](https://hackage.haskell.org/package/base-4.16.1.0/docs/Prelude.html#g:9)*

```haskell
class Semigroup a => Monoid a where
```

Methods
- `mempty`
- `mappend`
- `mconcat`