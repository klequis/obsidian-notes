So I know that there is an instance of `Functor` for `Maybe`. I guess that means you can use `fmap` for any `Maybe` type?

```haskell
fmap :: (a -> b) -> f a -> f b
```

@CarstenK Thank you for the `fmap` version. I haden't thought of that at all. I do have a question on how it works.

The overall goal of the program is to enter an equation as a string such as "1 + 2" or "1 * 2*" and get back the answer to the equation. For the moment I'm just woking on determining the operation. Here is what I have so far:

**simpleCalcCarstenK2.hs**
```haskell
data Operations = Addition | Subtraction | Multiplication | Division | Modulus
  deriving (Show, Eq)

getSymbol:: Maybe Operations -> Maybe Char
getSymbol = fmap getSymbol'

getSymbol' :: Operations -> Char
getSymbol' Addition = '+'
getSymbol' Subtraction = '-'
getSymbol' Multiplication = '*'
getSymbol' Division = '/'
getSymbol' Modulus = '%'

getOperation :: Foldable t => t Char -> Maybe Operations
getOperation str
| '+' `elem` str = Just Addition
| '-' `elem` str = Just Subtraction
| '*' `elem` str = Just Multiplication
| '/' `elem` str = Just Division
| '%' `elem` str = Just Modulus
| otherwise = Nothing

main :: IO ()
main = do
  userInput <- getContents
  print userInput
  let operation = getOperation userInput
  let b = getSymbol operation
  print b
```

```
> ghc simpleCalcCarstenK2.hs
> ./simpleCalcCarstenK2
1 + 2
"1 + 2\n"
-- press ctrl-d
Just '+'
```

So that is all good. Here is the question:

- I'm passing to `symbol` a `Maybe Operations`
- But `getSymbol'` is expecting `Operations`.
- It appears that `getSymbol` is passing an `Maybe Operations` to `getSymbol'` but a `Operations` is expected. 
- What is really happening there?
