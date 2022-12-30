# #maxBound, #minbound 

Using the built-in type `Char`

```haskell
> maxBound :: Char
'\1114111'

> minBound :: Char
'\NUL'
```

## Example

Using a user-defined data type
```haskell
data Direction = North | East | South | West
  deriving (Bounded, Show)

> minBound :: Direction
North
```

- You only need to derive `Bounded`. `Show` is to print the value.

## Other classes commonly derived
However, it is likely you want to derive other classes as well such as `Eq` & `Enum`.

### Enum
`Enum` will allow you to use `toEnum` & `fromEnum` as well as several others.

```haskell
> fromEnum South
2 -- the second data constructor

> toEnum 2 :: Direction
South
```

### Eq

`Eq` does what you would expect.

```haskell
> North == North
True

> North == South
False
ghci> toEnum South
```