# #record-syntax
- Record **fields are functions** that go from a product type to a member of product
- 

## Example 1
```haskell
data Person = Person { 
    name :: String
  , age :: Int }
  deriving (Eq, Show)
```

```haskell
Person {name = "Papu", age = 5}
```

```haskell
> papu = Person "Papu" 5
> age papu  -- just like a function bec is one
5
> name papu -- just like a function bec is one
"Papu"
```

## Example 2
```haskell
data Name = Name {
    firstName :: String
  , lastName :: String
}

instance Show Name where
  show (Name first last) = mconcat [first, " ", last]

data Student = Student {
    studentId :: Int
  , gradeLevel :: GradeLevel
  , studentName :: Name
} deriving Show

students :: [Student]
students =  [Student 1 Senior (Name "Audre" "Lorde")
            ,Student 2 Junior (Name "Leslie" "Silko")
            ,Student 3 Freshman (Name "Judith" "Butler")
            ,Student 4 Senior (Name "Guy" "Debord")
            ,Student 5 Sophmore (Name "Jean" "Baudrillard")
            ,Student 6 Junior (Name "Julia" "Kristeva")]
```

> Record fields are functions

```haskell
> map studentId students
[1,2,3,4,5,6]

> map studentName students
[Audre Lorde,Leslie Silko,Judith Butler,Guy Debord,Jean Baudrillard,Julia Kristeva]

> map (firstName . studentName) students
["Audre","Leslie","Judith","Guy","Jean","Julia"]
```