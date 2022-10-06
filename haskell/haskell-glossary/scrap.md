
```haskell
λ> import Data.Monoid 

λ> (mempty :: Int -> [Bool]) 123 
[] 
-- In this case mempty refers to a function that ignores its Int argument and returns mempty :: [Bool] which is empty list 

λ> (mempty :: String -> Any) "abc" 
Any {getAny = False} 
-- The function ignores its argument and returns mempty :: Any 
-- A more complex example showing chaining of multiple Monoid instances 
-- Monoid () 
-- Monoid () => Monoid (IO ()) 
-- Monoid (IO ()) => Monoid (Bool -> IO ()) 
-- Monoid (Bool -> IO ()) => Monoid (Int -> Bool -> IO ()) 
-- Monoid (Int -> Bool -> IO ()) => Monoid (String -> Int -> Bool -> IO ()) 
λ> (mempty :: String -> Int -> Bool -> IO ())
"hi" 1 True 
λ> -- ^ This function is basically equivalent to (\str int bool -> return ()) 
-- this stuff can sometimes save you from typing few characters :-)

```