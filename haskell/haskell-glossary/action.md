# #action

- Used to do things that have side effects
- `IO` is an action
- not all actions are `IO`
- Actions do not take any arguments.
- Actions specify something that *can be* done.
- Actions will not do anything until you "run" them

`putStrLn` is not an action because it takes an argument:
```haskell
:t putStrLn
putStrLn :: String -> IO ()
```
`putStrLn "hi"` is because it doesn't take any actions:
```haskell
putStrLn "hi"
putStrLn "hi" :: IO ()
```

> -  So `putStrLn` is a function that takes one argument and returns an `IO.
> - `putStrLn "hi"` is an action.

## Using `IO` as an example

- `IO` actions can result in a value that can be used by a Haskell program.
- Values from the console are `String` so they are `IO String`
- All `IO` actions have the type `IO a`.


>  An I/O action is an action that, when performed, has side effects, including reading from input and printing to the screen, and will contain a return value. (HPFFP p.199)
