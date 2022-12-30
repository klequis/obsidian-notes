# Enumerating Types

```haskell
data Turn = TNone | TLeft | TRight | TAround
  deriving (Show, Enum)

> [TNone .. TAround]
[TNone,TLeft,TRight,TAround]

```

