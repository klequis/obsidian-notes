# Issues with numbers and text
## Coercing values

### Example 1

```haskell
circleArea :: Double -> Double
circleArea r = pi * r * r
```

`pi` is from the `Floating` type class so could make `circleArea` more generic as

```haskell
circleArea :: Floating a => a -> a
circleArea r = pi * r * r
``` 

But unfortunately this allows complex numbers which are not valid.

You can require that `r` be a real number but then you will get a type error because `pi` and `r` are different types.

```haskell
circleArea2 :: (Real a, Floating b) => a -> b
circleArea2 r = pi * r * r
-- Could not deduce (Floating a) arising from a use of ‘pi’
-- from the context: (Real a, Floating b)
```

You can see the problem in the hierarchy of number types below. 

![[Pasted image 20221014121309.png]]

-- Solution is to convert `r * r` to the appropriate type using `realToFrac`

```haskell
realToFrac :: (Real a, Fractional b) => a -> b
```

```haskell
circleArea3 :: (Real a, Floating b) => a -> b
circleArea3 r = pi * realToFrac (r * r)
```

### Example 2

```haskell
meanOfList2 xs = (sum xs) / (length xs)
-- Could not deduce (Fractional Int) arising from a use of ‘/’
```

To solve this use `fromIntegral`:
```haskell
fromIntegral :: (Integral a, Num b) => a -> b
```

```haskell
meanOfList3 xs = fromIntegral (sum xs) / fromIntegral (length xs)
```


```haskell
xs :: [Int]
xs = [1,2,3,4,5]

meanOfList1 :: Foldable t => t Int -> Int
meanOfList1 xs = sum xs `div` length xs
```

>[!WARNING]
>
The fromIntegral function is actually unsafe and should be used
with care. The problem is that this function doesn’t check for underflows and overflows in data. This could potentially lead to corrupting data when doing incompatible type conversion.

### Note: `(/)` is not `div`

`(/)` is `Fractional` division.
```haskell
> :t (/)
(/) :: Fractional a => a -> a -> a
```

`div` is `Integer` division and is **truncated** toward negative infinity

```haskell
ghci> :t div
div :: Integral a => a -> a -> a
```


## Computing with fixed precision

### `Data.Fixed`

- Provides types with 0, 1, 2, 3, 6, 9 & 12 places out of the box.
- Names are `Deci`, `Milli`, `Pico`, etc.

```haskell
import Data.Fixed

data E4

instance HasResolution E4 where
  resolution _ = 10000

type Fixed4 = Fixed E4

```

```haskell
> pi = 3.14 :: Fixed4
> pi
3.1400 -- preserves 4 digits after .
> resolution pi
10000 -- gives the resolution of `pi`
>  e = 2.7182818 :: Fixed4
> e
2.7182 -- truncates / does not round
> 
```

> [!WARNING]
> Numbers are truncated, not rounded.

## Resolution

Resolution is 10 to the power of desired number of digits after the decimal point. So for 4 decimal places it is $10^4$ or $10,000$


## Built-in types

While `Data.Fixed` doesn't have a type with 4 decimal places it does have those noted above and can be used as follows.

### `Milli` 3 decimal places

```haskell
> a = 10.000 :: Milli
> a
10.000
> b = 10 :: Milli
> b
10.000
> c = 10 : E6
```

### `Micro` 6 decimal places

```haskell
> a = 10 :: Micro
> a
10.000000
> b = 10.0000009 :: Micro
> b
10.000000

```

## Safe Alternatives for the `Read` Function

The problem with `Read` is that it raises an exception whenever it can parse something. You can use `readMaybe` and `readEither` as safe alternatives to `Read`.

## The OverloadedStrings GHC extension with user-defined types

*(p. 45)*

Types like Person can be easily used with the OverloadedStrings GHC extension. To do that, we have to implement the IsString type class as follows:
```haskell
instance IsString Person where
  fromString name = Person name Nothing
```

With this instance and after enabling the extension, we could write the following definition of the spj constant:
```haskell
spj :: Person
spj = "Simon Peyton Jones"
```

**My interpretation:** User-defined types won't automatically work with OverloadedStrings. To get them to do so you create an instance of `IsString` for the user-defined type.

## Compatibility between `Show` and `Read`

Generally, a `String` produced by `Show` can be read by `Read`. This is not enforced by the compiler but holds true for every standard type.
`
Implementing Show manually (i.e., making an instance instead of deriving) breaks compatibility with `Read`.

## TextShow Example

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Person where

import Data.String
import TextShow ( fromString, printT, TextShow(showb) )

data Person = Person String (Maybe Int)

instance IsString Person where
  fromString name = Person name Nothing

instance TextShow Person where
  showb (Person name Nothing) = TextShow.fromString name
  showb (Person name (Just age)) =
    TextShow.fromString name <> " (" <> showb age <> ")"

homer :: Person
homer = Person "Homer Simpson" (Just 39)

spj :: Person
spj = "Simon Peyton Jones" -- (Just 5)

main :: IO ()
main = do
  printT homer
  printT spj
```

Of interest in this example
- `instance IsString Person` does not provide a `Show` instance. You need to implement one.
- `instance TextShow Person` does.
- `printT` must be used. `print` will still say there is no instance of `Show`

