### Imports
```haskell
import Data.List
import qualified Data.Map as Map
import Data.Semigroup
import Data.Maybe
```

> [!QUESTION] 
> Why `Data.Map` and not just `map`
> Because `Data.Map` is a container not a function

### Create a timeseries type
```haskell
data TS a = TS [Int] [Maybe a]
```

### Aside
- Say I read in 4 files 
- And I got `[file1,file2,file3,file4]`
- Now I want to transform each file into a `TS a`
- The book uses fileToTS

```haskell
fileToTS :: [(Int,a)] -> TS a
fileToTS tvPairs = createTS times values
  where (times, values) = unzip tvPairs
```

- It uses `createTS`
```haskell
createTS :: [Int] -> [a] -> TS a
createTS times values = TS completeTimes extendedValues
where completeTimes = [(minimum times) .. (maximum times)]
      timeValueMap = Map.fromList (zip times values)
      extendedValues = map (\v -> Map.lookup v timeValueMap) completeTimes
```