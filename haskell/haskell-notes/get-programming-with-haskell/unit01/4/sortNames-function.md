I'm working on the `sortNames` program. Was trying to split `[String]` contents "First|Last".

- It seemed the best way to do this is using `Data.Text`. 
- My first instinct was to use `pack` and `unpack` with `splitOn` but had not luck.
- After some reading learned you can avoid `pack` & `unpack` buy using the language pragma `{-# LANGUAGE OverloadedStrings #-}`. Doing so makes the following code work.

```haskell
main :: IO ()
main = do
  print (T.splitOn "|" "First|Last")
```

- Without it I got:

```
• Couldn't match type ‘[Char]’ with ‘T.Text’
      Expected: T.Text
        Actual: String
    • In the second argument of ‘T.splitOn’, namely ‘"First|Last"’
      In the first argument of ‘print’, namely
        ‘(T.splitOn "|" "First|Last")’
      In a stmt of a 'do' block: print (T.splitOn "|" "First|Last")
   |
14 |     print (T.splitOn "|" "First|Last")

```

From the "Couldn't match type `[Char]` with `T.Text` I'm guessing that Haskell is taking care of the converstion from `[Char]` to `T.text` for you. However, I have read there are performance problems with using `pack` & `unpack` to convert between `[Char]` & `T.Text` which leaves an open question, i.e., I doubt they would introduce a performance issue as a standard solution.

> I learned how to get things working by looking in one of the books I have. Too bad I spent 45 minutes fishing before looking :(

