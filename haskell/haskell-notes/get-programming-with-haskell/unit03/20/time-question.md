How to create instance of Show for data Time a = Time (Int,Double)

```haskell
data Time a = Time (Int,Double)

time1 :: Time a
time1 = Time (1,2.5)
```


I tried
```haskell
instance Show (Time a) where
  show (Time v) = (read t) ++ (read v)
    where t = fst v
          v = snd v
```

and
```haskell
instance Show (Time a) where
  show (Time tup) = read (t,v)
    where t = fst tup
          v = snd tup
```
---



I want to create a type `Times` that is a list of `Times`. 

I have 4 lists of `[(Int,Double)]` where `Int` is a proxy for time and double is some value.

So a `Time` would be one `(Int,Double)`.

Than I want to define `Times` as a list of `Time` but can't get that to work.

```haskell
data Time a = Time (Int,Double)
data Times a = Times [Time a]

time1 :: Time a
time1 = Time (1,2.5)

time2 :: Time a
time2 = Time (2,3.5)
```


I thought I should be able to do
```haskell
times :: Times a
times = [time1, time2]
```

But that gives the error
```
Couldn't match expected type ‘Times a’  
  with actual type ‘[Time a0]’
```

```haskell

times :: [Time a]
times = [time1, time2]
```

## Answer

I don't totally remember the above issue but here is the solution I worked out. 
```haskell
fileToTimes :: p -> Times a
fileToTimes file = Times (map Time file1)

instance Show (Time a) where
  show (Time (t,v)) = "(" ++ show t ++ ", " ++ show v ++ ")"

instance Show (Times a) where
  show (Times time) = concatMap (\(Time t) -> show t) time
```

There were two realizations:

- First, a `Time` could be destructured in the lambda.
- Second, I was using `read` but needed to use `show`.
```haskell
instance Show (Times a) where
  show (Times time) = concatMap (\(Time t) -> show t) time
```


