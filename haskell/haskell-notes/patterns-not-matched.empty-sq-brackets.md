# Patterns not matched: []

> [!WARNING] Going to leave this an open question for now.

```haskell
testHeadA (x:xs)
  | x == LParen = True
  | x == Number = True
  | otherwise = False
-- ...Patterns not matched: []
```

```haskell
testHeadB [] = False
testHeadB (x:xs)
  | x == LParen = True
  | x == Number = True
  | otherwise = False
```

I was pretty confused why `testHeadA` gives the warning "... Patterns not matched: []". Given I covered the true and false case I thought that was all cases (True || False). But to avoid the warning one needs to add a check for [] as done in `testHeadB`

It is even more confusing since this works:
```haskell
testHeadB [Nothing,Just Number]
False
```
