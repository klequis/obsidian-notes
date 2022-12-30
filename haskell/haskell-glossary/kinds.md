#kinds

*PHFFP p. 435*

> [!IMPORTANT] Kinds are the types of type constructors
> They primarily encode the number of arguments they take

- The kind of a #concrete-type is expressed as `*`
- The default kind in Hasekll is `*`

> [!IMPORTANT] A kind is not a type until it is fully applied


Examples
- `*` - a type that takes no parameters. It is a concrete type.
- `* -> *` - a type that takes one parameter (aka "applied once").
- `* -> * -> *` - a type that takes two parameters (aka "applied twice").

You can use GHCi to inspect a types kind:
```haskell
GHCi> :kind Int
Int :: *

GHCi> :kind (,)
(,) :: * -> * -> *
```

- A type with kind `* -> *` is awaiting application to a type constant of kind `*`.

- A type is a fully applied concrete type when it is represented as `*`.

Types are applied from the left

```haskell
data SomeType a b c = SomeType a b c

> :k SomeType
SomeType :: * -> * -> * -> *
> :k SomeType Int
SomeType Int :: * -> * -> *
> :k SomeType Int String
SomeType Int String :: * -> *
> :k SomeType Int String Bool
SomeType Int String Bool :: *
```




## References
- [Haskell Programming from first principles ](https://haskellbook.com/)