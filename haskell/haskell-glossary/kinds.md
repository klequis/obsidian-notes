# #kinds

- Kinds are the types of types.
- The *kind of a type* indicates the number of parameters the type takes and is expressed as `*`.

Examples
- `*` - a type that takes no parameters. It is a concrete type.
- `* -> *` - a type that takes one parameter.
- `* -> * -> *` - a type that takes two parameters.

You can use GHCi to inspect a types kind:
```haskell
GHCi> :kind Int
Int :: *

GHCi> :kind (,)
(,) :: * -> * -> *
```

- A type with kind `* -> *` is awaiting application to a type constant of kind `*`.

- A type is a fully applied concrete type when it is represented as `*`.

