It is common to come across error messages that have `a0` in them (or similar type variables `p1`, `t0`, etc.) and I am always wondering what specifically they refer to. Here is an example.

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Expr1 where

import TextShow ( showbParen, Builder, TextShow(showb, showbPrec) )

data Expr a = Lit a
            | Add (Expr a) (Expr a)
            | Mult (Expr a) (Expr a)
  deriving (Show, Read)

instance TextShow a => TextShow (Expr a) where
  showbPrec :: Int -> Expr a -> Builder
  showbPrec p e =
    case e of
      Lit a -> showb a
      Add e1 e2 -> showbHelper p 5 "+" e1 e2
      Mult e1 e2 -> showbHelper p 6 "*" e1 e2

showbHelper :: (TextShow a)  => Int -> Int -> Builder -> Expr a -> Expr a -> Builder
showbHelper outerPrec thisPrec op e1 e2 =
  showbParen (outerPrec > thisPrec)
  $ showbPrec thisPrec e1 <> op <> showbPrec thisPrec e2
```

```haskell
plus :: Builder
plus = "+"

expr1 :: Expr Integer
expr1 = Add (Lit 2) (Lit 3)

expr2 :: Expr Integer
expr2 = Mult (Lit 4) (Lit 5)

bb1 = showbParen (True) $  showbPrec 6 expr1 
                        <> plus 
                        <> showbPrec 5 expr2
```

All is good here but I wanted to substitute in the value for `expr1`  and `expr2` and tried this first:

```haskell
bb1 = showbParen (True) 
        $  showbPrec 6 (Add (Lit 2) (Lit 3))
        <> plus 
        <> showbPrec 5 expr2
```

And got the below error with the entire line `showbPrec 6 (Add (Lit 2) (Lit 3))` underlined. *(I'm using VSCode with HLS)*

```
• Ambiguous type variable ‘a0’ arising from a use of ‘showbPrec’  
prevents the constraint ‘(TextShow a0)’ from being solved.  
Probable fix: use a type annotation to specify what ‘a0’ should be.  
These potential instances exist:  
instance (TextShow a, TextShow b) => TextShow (Either a b)  
-- Defined in ‘TextShow.Data.Either’  
instance TextShow Ordering -- Defined in ‘TextShow.Data.Ord’  
instance TextShow a => TextShow (Expr a)  
-- Defined at /home/klequis/d/learn/haskell/book/haskell-in-depth/mini-proj/expr/app/Expr1.hs:13:10  
...plus 26 others  
...plus 208 instances involving out-of-scope types  
(use -fprint-potential-instances to see them all)  
• In the first argument of ‘(<>)’, namely  
‘showbPrec 6 (Add (Lit 2) (Lit 3))’  
In the second argument of ‘($)’, namely  
‘showbPrec 6 (Add (Lit 2) (Lit 3)) <> plus <> showbPrec 5 expr2’  
In the expression:  
showbParen (True)  
$ showbPrec 6 (Add (Lit 2) (Lit 3)) <> plus <> showbPrec 5 expr2typecheck(-Wdeferred-type-errors)
```

It wasn't immediately clear to me what `a0` was. 

Only figuring out the fix is adding the type for `Add (Lit 2) (Lit 3)`  -- [A]  ...

```haskell
bb1 = showbParen (True) 
        $  showbPrec 6 (Add (Lit 2) (Lit 3) :: Expr Int) -- [A]
        <> plus 
        <> showbPrec 5 expr2
```

... did I see that `a0` is probably the second argument to `showbPrec` **(is that correct?)**

```haskell
:t showbPrec
showbPrec :: TextShow a => Int -> a -> Builder
```

Is that the process for figuring out the error message or am I missing something that makes it more straight forward to know what `a0` is?

