# #reduce 
A type declared with `newtype` has one and only one unary data constructor.

At runtime, the difference between the `newtype` and the type is stripped away and it is the type that was contained.

**Example**

```haskell
`newType Goats = Goats Int`
```

Similar to a type alias, the type of `Goats` is `Int`. Before compile `Goats` is like a alias for `Int`.

However, unlike a type alias you can define instances for a `newtype` such that you can give the `newtype` behavior that the type it contains does not have.

```haskell
newtype Goats = Goats Int deriving Show

instance TooMany Goats where
  tooMany (Goats n) = n > 43
```

`Goats` is an `Int` but it has a `tooMany`  method that `Int` does not.

 > A `newtype` is merely a way to direct the compiler to choose the correct instance. (Book of Moands p.8)
 
 

