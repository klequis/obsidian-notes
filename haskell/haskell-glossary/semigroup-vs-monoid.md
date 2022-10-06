# Semigroup vs Monoid

```haskell
class Semigroup a => Monoid a where

class Semigroup a where
```

## Primary Methods

| method | semigroup | monoid | example / note |
| - | - | - | - |
| `mempty` | | Y | identity |
| `mappend` | | Y | Same as <> <sup>(1)</sup>  |
| `mconcat` |  | Y | `moncat ["Hello", " ", "Haskell", "!"]` -- "Hello Haskell!" |
| `<>` | Y |  | `[1,2,3] <> [4,5,6]` -- [1,2,3,4,5,6] ``|

(1) Will be removed it future release of Haskell. `<>` from `Semigroup` will be used instead.




