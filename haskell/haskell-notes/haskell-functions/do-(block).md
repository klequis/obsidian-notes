# #do #do-block

- A do-block combines together two or more actions into a single action.

- The result of a do-block is the result of the last action in the block


## Example1

```haskell
main :: IO ()
main = do
  line <- getLine
  print line
```

While `getLine` is type `IO String`, `line` is type `String`.

---

You can put multiple actions the the `then` or `else` of an `if` by nesting a `do`.
```haskell
main :: IO ()
main = do
    line <- promptLine "What do you want? "   -- line :: String
    if line == "wisdom"
        then putStrLn "No man is without enemies."
        else do -- nested do-block
            putStrLn ("I don't have any " ++ line)
            putStrLn "Perhaps you want some wisdom?"
```