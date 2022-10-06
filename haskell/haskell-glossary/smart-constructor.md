# #smart-constructor

Allows you to construct values of a type only when they meet certain criteria.

**Example**
```haskell
mkPerson :: Name -> Age -> Maybe Person
mkPerson name age
  | name /= "" && age >= 0 = Just $ Person name age
  | otherwise = Nothing
```