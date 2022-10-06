
```haskell
sqlEx1 :: T.Text
sqlEx1 = "SELECT firstName, lastName FROM students"

sqlEx2 :: T.Text
sqlEx2 = "SELECT firstName, lastName FROM students WHERE firstName == John"
```

- Does it have 1 SELECT, 1 From and 1 WHERE
```haskell
length (indices "select" sqlEx1) == 1
length (indices "from" sqlEx1) == 1
length (indices "where" sqlEx1) is 0 || 1
```

- Does it have >= 1 select field
- Does it have >= 1 from field


