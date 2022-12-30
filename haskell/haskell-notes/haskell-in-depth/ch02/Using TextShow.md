
- `instance TextShow a => TextShow (Expr a` defines how to "show" a value of type `Expr`
- Methods of `TextShow` that are implemented are
    - `showbPrec`
    - `showbParen`

## `showbParen`

Surrounds Builder output with parentheses if the Bool parameter is True.

```haskell
showbParen :: Bool -> Builder -> Builder
```

## `showbPrec`

Convert a value to a Builder with the given precedence.

```haskell
showbPrec :: TextShow a => Int -> a -> Builder
```

## Questions

Why this difference. The same expression `(Lit 2)`  is sometimes an `Expr Integer` and others `Num a => Expr a`. It seems when defined in a file type inference provides a more specific type. Is it just a GHCi thing?

```haskell
> :t ex3 -- ex3 = (Lit 2)
ex3 :: Expr Integer

> :t (Lit 2)
(Lit 2) :: Num a => Expr a

> ex99 = (Lit 2)
> :t ex99
ex99 :: Num a => Expr a

```


## Seriously not understanding how this code works

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Expr1 where

import TextShow

data Expr a = Lit a
            | Add (Expr a) (Expr a)
            | Mult (Expr a) (Expr a)
  deriving (Show, Read)

instance TextShow a => TextShow (Expr a) where
  showbPrec :: TextShow a => Int -> Expr a -> Builder
  showbPrec p e =
    case e of
      Lit a -> showb a
      Add e1 e2 -> showbHelper p 5 "+" e1 e2
      Mult e1 e2 -> showbHelper p 6 "*" e1 e2
    
showbHelper outerPrec thisPrec op e1 e2 =
  showbParen (outerPrec > thisPrec)
  $ showbPrec thisPrec e1 <> op <> showbPrec thisPrec e2

ex3 = (Lit 2)

ex4 = Add (Lit 2) (Lit 2)

ex5 = Mult (Add (Lit 2) (Lit 2)) (Lit 3)

ex6 = Mult (Add (Lit 'a') (Lit 'b')) (Lit 'c')
```

Usage

```haskell
ghci> printT ex5
(2+2)*3
```

Looking at the instance
```haskell
instance TextShow a => TextShow (Expr a) where
  showbPrec :: TextShow a => Int -> Expr a -> Builder
  showbPrec p e =
    case e of
      Lit a -> showb a
      Add e1 e2 -> showbHelper p 5 "+" e1 e2
      Mult e1 e2 -> showbHelper p 6 "*" e1 e2
```

```haskell
> :t showbPrec
showbPrec :: TextShow a => Int -> a -> Builder
```

So, `showbPrec` takes
- `Int`
- `TextShow a`
And returns
- `Builder`

## Question

# Workings of type inference in specific case

I'm hesitant to ask this question but will because I want to be sure I get it.

```haskell
import TextShow ( showbParen, Builder, TextShow(showb, showbPrec) )

data Expr a = Lit a
            | Add (Expr a) (Expr a)
            | Mult (Expr a) (Expr a)
  deriving (Show, Read)

instance TextShow a => TextShow (Expr a) where
  showbPrec :: TextShow a => Int -> Expr a -> Builder
  showbPrec p e =
    case e of
      Lit a -> showb a
      Add e1 e2 -> showbHelper p 5 "+" e1 e2
      Mult e1 e2 -> showbHelper p 6 "*" e1 e2

showbHelper :: (TextShow a)  => Int -> Int -> Builder -> Expr a -> Expr a -> Builder
showbHelper outerPrec thisPrec op e1 e2 =
  showbParen (outerPrec > thisPrec)
  $ showbPrec thisPrec e1 <> op <> showbPrec thisPrec e2

ex5 = Mult (Add (Lit 2) (Lit 2)) (Lit 3)
```

Usage
```haskell
ghci> printT ex5
(2+2)*3
```

It seems to me that `"+"` & `"*"` in the below 2 lines is a `Builder` because `showbHelper` signature expects it to be?

And it seems that `"+"` & `"*"` are `String` but somehow get transformed to `Builder`? - I'll guess the compiler does that?

```haskell
Add e1 e2 -> showbHelper p 5 "+" e1 e2
Mult e1 e2 -> showbHelper p 6 "*" e1 e2
```

![[Pasted image 20221024191458.png]]