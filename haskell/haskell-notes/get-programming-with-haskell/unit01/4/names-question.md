I was trying to figure out how to create a data type that represented a tuple pair of two strings. I did but it doesn't work as I expected.

Each name will be `("John", "Candell")`, so `([Char], [Char])`

So I created

```haskell
data Name a = Name ([Char], [Char]) deriving (Show, Eq, Ord)
```

So now I can have
```haskell
x = Name ("John", "Candell")
```

Then I wanted to have an an array of name: `[Name]`. So I tried

```haskell
names :: [Name a]
names = [ ("Ian", "Curtis")
        , ("Bernard","Sumner")
        , ("Peter", "Hook")
        , ("Stephen","Morris")
        ]
```

and again with the type signature `names :: [Name]`. Both produce errors.

This works and seems reasonable if I were reading the names from
seems a but if I were reading from a file or database I would have to

This works
```haskell
names :: [Name a]
names = [ Name ("Ian", "Curtis")
        , Name ("Bernard","Sumner")
        , Name ("Peter", "Hook")
        , Name ("Stephen","Morris")
        ]
```

and if I were reading names from a file

```
"Ian|Curtis"
"Bernamed|Summer"
```

it makes sense. But I'm not doing that.