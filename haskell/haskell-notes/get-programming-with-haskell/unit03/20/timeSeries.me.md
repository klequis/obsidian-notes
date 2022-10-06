
# timeSeries.hs
> [!WARNING] Does not match book code
> My code differs from the book's code as I missed a step. However, it works and perhaps a bit better since it works with `file[x]` and there is no need to hardcode `ts1` to `ts4`.

- We want to combine several sets of data. Combining is a job for `Semigroup`.
- Additionally, implementing `Monoid` will will allow `mconcat` to be used.

> [!DANGER] Am uncertain about the above
> The above is confusing because it isn't clear to me what `Semigroup` + `Monoid` can do vs. `Semigrop` alone other than `Monoid` will provide `mconcat`.


## Flow

```
-- THIS IS INCOMPLETE
filesToTS -> createTS
filesToTS <- createTS
```

## Analysis

**`data TS a`**
```haskell
data TS a = TS [Int] [Maybe a]
```


---

**`showTVPair`**
```haskell
showTVPair :: Show a => Int -> (Maybe a) -> String
showTVPair time (Just value) = mconcat [show time,"|",show value,"\n"]
showTVPair time Nothing = mconcat [show time,"|NA\n"]
```

- Called by the `Show TS a` instance.
- If sent a `Just a` that has a value it will concat to string of e.g., "1|200.1
"
- Assumes `time` is never empty.
---

**`instance Show (TS a)`**
```haskell
instance Show a => Show (TS a) where
	show (TS times values) = mconcat rows
		where rows = zipWith showTVPair times values
```

- This is an instance of `Show` for type `TS a`.
- It has a `show` method which will be called by haskell

---

**`createTS`:** Takes a list of times and a list of values and creates a `TS` type.
```haskell
createTS :: [Int] -> [a] -> TS a
createTS times values = TS completeTimes extendedValues
	where completeTimes = [minimum times .. maximum times]
		  timeValueMap = Map.fromList (zip times values)
		  extendedValues = map (\v -> Map.lookup v timeValueMap) 
                                    completeTimes
```
- `completeTimes` creates a list of times with no missing values.
- `timeValueMap` creates a `Map` from the re-zipped time/value pairs.
- `extendeValues` `map`s over `completedtimes` and looks-up each value in `timeValueMap`. 
	- Since `Map.lookup` returns a `Maybe` the result list will be a list of `Just` and `Nothing`: `[Just 200.1, Nothing, Just 199.4, ...]`
- Finally, `createTS` returns a `TS` of `completedTimes` and `extendedValues`.


```haskell
-- Map.lookup signature
lookup :: Ord k => k -> Map k a -> Maybe a

times = [1,2,3,4,5,6,9,10,12]
value = [200.1,199.5,199.4,198.9,199.0,200.2,200.3,201.2,202.9]

> completeTimes
[1,2,3,4,5,6,7,8,9,10,11,12]
-- creates array with every num between min and max

> timeValueMap
fromList [(1,200.1),(2,199.5),(3,199.4),(4,198.9),(5,199.0),(6,200.2),(9,200.3),(10,201.2),(12,202.9)]

> extendedValues = map (\v -> Map.lookup v timeValueMap) completeTimes
> extendedValues
[Just 200.1,Just 199.5,Just 199.4,Just 198.9,Just 199.0,Just 200.2,Nothing,Nothing,Just 200.3,Just 201.2,Nothing,Just 202.9]
-- Map.loolup returns a Maybe
```

---

**`filesToTS`:** Unzip the paris into times and values.
```haskell
fileToTS :: [(Int,a)] -> TS a
fileToTS tvPairs = createTS times values
	where (times, values) = unzip tvPairs
```

- `(times, values) = ...` creates two variables
- These variables are pased to `CreateTS`

```haskell
> (times,values) = unzip file1

> times
[1,2,3,4,5,6,9,10,12]

> values
[200.1,199.5,199.4,198.9,199.0,200.2,200.3,201.2,202.9]
```

---

**`insertMaybePair`**
```haskell
insertMaybePair :: Ord k => Map.Map k v -> (k, Maybe v) -> Map.Map k v
insertMaybePair myMap (_,Nothing) = myMap
insertMaybePair myMap (key,(Just value)) = Map.insert key value myMap
```

- `myMap (_, Nothing)`: If the value is `Nothing` don't add it. Return `myMap` as is.
- Otherwise destructure the key and value using `(key,(Just value))`
- Then add `key` /`value` pair to `myMap`

---

**`combineTS`**
```haskell
combineTS :: TS a -> TS a -> TS a
combineTS (TS [] []) ts2 = ts2
combineTS ts1 (TS [] []) = ts1
combineTS (TS t1 v1) (TS t2 v2) = TS completeTimes combinedValues
where combinedTimes = mconcat [t1,t2]
      completeTimes = [(minimum t1) .. (maximum t2)]
      tvMap = foldl insertMaybePair Map.empty (zip t1 v1)
      updatedMap = foldl insertMaybePair tvMap (zip t2 v2)
      combinedValues = map (\v -> Map.lookup v updatedMap) 
                                                completeTimes
```

- Pattern matching logic
  - If first `TS` is empty but second isn't,      -> return second.
  - If first `TS` is not empty but second is,    -> return first
  - If neither first or second `TS` are empty, -> combine them

**Make it an instance of `Semigroup`**

At this point, if you create `ts1` & `ts2` manually using `fileToTS <filename>` you can call `combineTS`.

```haskell
> ts1 = createTS file1
> ts2 = createTS file2
> combineTS ts1 ts2
1|200.1
2|199.5
3|199.4
...
```

But you are limited to combining two files. If you make `TS a` an instance of `Semigroup` then you can use `<>` to combine multiple sets.

```haskell
instance Semigroup (TS a) where
  (<>) = combineTS
```

```haskell
> file1 <> file2 <> file3
[(1,200.1),(2,199.5), ... (25,218.7)]
```


**Make it an instance of `Monoid`**

Now you can combine multiple files using `<>` but have no easy way to combine a list of files such as: `[file1, file2, file3, file4]`.

To enable this, make TS an instance of `Monoid`.

```haskell
instance Monoid (TS a) where
  mempty = TS [] []  -- identity
  mappend = (<>)     -- monoid append

```

```haskell
> mconcat [file1, file2, file3]
[(1,200.1),(2,199.5), ... (25,218.7)]
```

**`tsAll`**
```haskell
tsAll :: TS Double
tsAll = mconcat [file1,file2,file3,file4]
```

## Section 20.3.1 Calculating the min and max values for your time series

**`type CompareFunc`**
```haskell
type CompareFunc a = (a -> a -> a)
```

- type that takes **????????????????**

**`type TSCompareFunc`**
```haskell
type TSCompareFunc a = ((Int, Maybe a) -> (Int, Maybe a) -> (Int, Maybe a))
```

- type that takes a function
- which takes 2 `(Int, Maybe a)` and returns a `(Int, Maybe a)`

```haskell
makeTSCompare :: Eq a => CompareFunc a -> TSCompareFunc a
makeTSCompare func = newFunc
  where newFunc (i1, Nothing) (i2, Nothing) = (i1, Nothing)
  newFunc (_, Nothing) (i, val) = (i,val)
  newFunc (i, val) (_, Nothing) = (i,val)
  newFunc (i1,Just val1) (i2,Just val2) = if (func val1 val2) == val1
                                      then (i1, Just val1)
                                      else (i2, Just val2)
```

- Takes a `CompareFunc`. Something like `makeTSCompare max (3,Just 200) (4,Just 10)`
- Two pattern matches checking for empty values
- Third pattern match assumes all values are present
- Uses the passed in function to see if `val1 == val2`.

> [!INFO] NOT DONE
> Have not finished with this section

