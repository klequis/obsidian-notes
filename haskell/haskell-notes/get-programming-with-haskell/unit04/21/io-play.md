
## Question: Is a value returned from `getLine` a `String` or an `IO String`

The type signature of `getLine` is `getLine :: IO String`. Therefore the answer is `IO String`.

But, according to [Get Programming with Haskell](https://www.manning.com/books/get-programming-with-haskell#:~:text=about%20the%20book,dive%20into%20custom%20Haskell%20modules.) **do-notation** allows you to treat `IO` types as if they were regular types. What does this really mean?

**Example 1**
```haskell
idIOStr :: IO String -> IO String
idIOStr a = a

idInt :: Int -> Int
idInt a = a

idStr :: String -> String
idStr a = a

main :: IO ()
main = do
  putStrLn "Enter an Int"
  a <- getLine
  -- let b = idIOStr a -- (A)
                       -- Expected: IO String,
                       -- Actual: String
  -- let b = idInt a   -- (B)
                       -- Expected: Int
                       -- Actual: String
  let b = idStr a      -- (C)
                       -- works
  putStrLn b
```

- Taking the assumption that, given the type signature of `getLine`, `a` is an `IO String`.
- (A) Trying to send `a` to a function that expects an `IO String` fails
- (B) Trying to send `a` to a function that expects an `Int` fails
- Conclusion: `a` is an `IO String` but *do-notation* allows it to be used as a `String`.


### Further
- The type signatue of `putStrLn` is `putStrLn :: String -> IO ()` 
- So if I were to try to give it an `Int` it would fail?

```haskell
main :: IO ()
main = do
  putStrLn "Enter an Int"
  a <- getLine
  let b = toInt a
  putStrLn b -- Expected: String  
             -- Actual: Int
```

Pretty clear, `putStrLn` expects a `String`.

