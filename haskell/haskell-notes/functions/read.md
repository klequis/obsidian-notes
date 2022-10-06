# #read

How does `read` work in this example?

```haskell
import Control.Monad ((>=>))

sayHi :: String -> IO String
sayHi greeting = do
  putStrLn greeting
  getLine

readM :: Read a => String -> IO a
readM = return . read

getAge :: String -> IO Int
getAge = sayHi >=> readM

askForAge :: IO Int
askForAge = getAge "Hello! How old are you? "
```

1. `askForAge` passes a `String` to `getAge`
2. `getAge` passes that `String` to `sayHi`
3. `sayHi` prints the `String`
4. `sayHi` gets input and returns `IO String` to `getAge`
5. In `getAge` `>=>` passes the `String` in `IO String` to `readM`
6. In `readM` first `read` converts `String` to `Int`
7. In `readM` second `return` lifts `Int` to `IO Int`
8. `IO Int` is returned to `getAge`
9. `IO Int` is printed to the console by ??