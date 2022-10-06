What happens as the number of `fmap`'s applied to a structure is increased from 1 to 3?

## Mapping over `[Maybe [Char]]`

Will be mapping over:
```haskell
lms = [Maybe [Char]]
lms = [Just "ave", Nothing, Just "woohoo"]
```

With the function:
```haskell
replacewithP :: b -> Char
replaceWithP = const 'p'
```

### One `fmap`
```haskell
> lms = [Just "ave", Nothing, Just "woohoo"]
> fmap replaceWithP lms
"ppp"
```

`fmap` lifted `replaceWithP` over the list and replaced the 3 elements contained within. Note that `"ppp"` is sill a `[]`

### Two `fmap`'s
```haskell
> lms = [Just "ave", Nothing, Just "woohoo"]
> (fmap . fmap) replaceWithP lms
[Just 'p', Nothing, Just 'p']
```

### Three `fmap`'s
```haskell
> lms = [Just "ave", Nothing, Just "woohoo"]
> (fmap . fmap . fmap) replaceWithP lms
[Just "ppp", Nothing, Just "pppppp"] 
```

## Mapping over `[Maybe [[Char]]]`

```haskell
lmls = [Just ["Ha", "Ha"], Nothing, Just []]
```

> [!WARNING] Not sure where I was going with the above. Leave it here for now.

## Another way of looking a lifting 1, 2, or 3 times.

Working with:
```haskell
lms = [Maybe [Char]]
lms = [Just "Ave", Nothing, Just "woohoo"]

replaceWithP = const 'p'
```

And:
```haskell
liftNaught = replaceWithP
```

### Lift Naught

`liftNaught`'s type will be inferred as `b -> Char`. But since we know we will be sending it a `lms` which is `[Maybe [Char]]` the type can be written as:
```haskell
[Maybe [Char]] -> Char
liftNaught = replaceWithP
```

And if used on `lms`
```haskell
> liftNaught lms
'p'
```

### Lift Once

Haskell will find this ambiguous:
```haskell
liftOnce = fmap replaceWithP
```

But we know it is:
```haskell
liftOnce :: [Maybe [Char]] -> [Char]
liftOnce = fmap liftNaught
```

And when used:
```haskell
> liftOnce lms
"ppp"
```

### Lift Twice

Again, we need to specify the type:
```haskell
liftTwice = [Maybe [Char]] -> [Maybe Char]
liftTwice = fmap liftOnce

> liftTwice lms
[Just 'p',Nothing,Just 'p']
```