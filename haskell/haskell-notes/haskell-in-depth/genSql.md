## Monoid

#monoid

-   A **set** of elements
	- any collection or group, not a formal Set.
-   A **binary operation** 
	- function that combines two elements and returns one element of the same set
-   A **neutral element** 
	- often called unit
	- 1 for multiplication
	- 0 for addition
	- `()` in Haskell

**Monoid Example**
- **set:** Natural numbers
- **binary operation:** `(+)`
- **neutral element:** 0

## writer

```haskell
writer :: (a, w) -> m a
```
`a` - actual result
`w` - accumulated result

One step before the final step of concatenation
```haskell
WriterT (Identity (
  [
    "INSERT INTO items VALUES ('Pen','Bob');\n",
	"",
	"INSERT INTO items VALUES ('Pencil','Alice');\n",
	"INSERT INTO items VALUES ('Book','Bob');\n",
	""
  ],
  [ 
    WrongFormat 2 "Glass:Mary:10",
	WrongFormat 5 "Bottle"
  ]
))
```

**Pragmas**

```haskell
So that "" will be interpreted as a `Text` value.
{-# LANGUAGE OverloadedStrings #-}
So the view pattern (_ : T.splitOn ":" -> [s1, s2])
{-# LANGUAGE ViewPatterns #-}
```

**Create two types**
```haskell
type SQL = Text

data ErrorMsg = WrongFormat Int Text
	deriving Show
```

**testData**

Some good and some bad records

```haskell
testData :: Text
testData = "Pen:Bob\nGlass:Mary:10\nPencil:Alice\nBook:Bob\nBottle"
```

**main**
- calls `testGenSql`

**testGenSql**

```haskell
-- explicitly run genSql passing it testData
let (sql, errors) = runWriter (genSQL testData) 
```

**genSQL**

- txt is broken into a list of individual lines
```haskell
T.lines txt
["Pen:Bob","Glass:Mary:10","Pencil:Alice","Book:Bob","Bottle"]
```

- then zipped into a tuple

```haskell
zip [1..] $ T.lines txt 
[(1,"Pen:Bob"),(2,"Glass:Mary:10"),(3,"Pencil:Alice"),(4,"Book:Bob"),(5,"Bottle")]
```

- traverse - traverses a data structure from left to right performing an action on each element
    - the data structure is the list of tuples
    - the action is the funciton `processLine`

**processLine**

Processes an individual line
```haskell
-- takes an Int and Text
-- returns a Writer
processLine :: (Int, Text) -> Writer [ErrorMsg] SQL
-- call genInsert which returns a String (the sql)
-- pure lifts the String into a functor
processLine (_, T.splitOn ":" -> [s1, s2]) = pure $ genInsert s1 s2
-- if first match fails
-- use tell to add the error
processLine (i, s) = tell [WrongFormat i s] >> pure ""
```

**back to: genSQL**

- <$> = `fmap`


```haskell
WriterT (Identity (

  "INSERT INTO items VALUES ('Pen','Bob');\nINSERT INTO items VALUES ('Pencil','Alice');\nINSERT INTO items VALUES ('Book','Bob');\n",
  
[WrongFormat 2 "Glass:Mary:10",WrongFormat 5 "Bottle"])

)

```