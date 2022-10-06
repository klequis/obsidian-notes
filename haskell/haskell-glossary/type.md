# #type 
>[!DANGER] Incomplete

Types are static and resolve at compile time.

Type and data constructors that take no arguments (nullary) are **constants**.
- Nullary type constructors are called *type constants*.
- Nullary data constructors are called *constant values*. 

 Types can be thought of as “an enumeration of constructors that have zero or more arguments.”

Although the term constructors is often used to describe all type and data constructors a distinction can be made between constructors and constants. 
- Type and data constructors that take no arguments are constants.
- So in `data Bool = True | False`, `Bool` is a constant, a concrete type. It isn’t waiting for any additional information in the form of an argument in order to be fully realized as a type. `True` and `False` are not being constructed in any meaningful sense.

In the Haskell Report type constructors that take no arguments are called **type constants**.

```Haskell
data UnaryTypeCon a = UnaryValueCon a
```

`UnaryTypeCon a` is a type constructor waiting for a concrete-type.

`UnaryValueCon a` is a data constructor waiting for a value to be applied.

## Inspecting types

Example type
```haskell
data PugType = PugData
```

When inspecting types with GHCi, you can:
- check the kind of a type constructor 
- check the type of a data constructor
```haskell
GHCi> :k PugType
PugType :: *

GHCi> :t PugData
PugData :: PugType
```

But you *cannot* check the type of a type constructor nor check the kind of a data constructor:
```haskell
GHCi> :t PugType
<interactive>:1:1: error: Data constructor not in scope: PugType
-- It isn't a type constructor, not a data constructor.

GHCi> :k PugData
<interactive>:1:1: error:
    Not in scope: type constructor or class ‘PugData’
    A data constructor of that name is in scope; did you mean DataKinds?
-- It is a data constructor, not a type constructor.
```


## An Example
This on got me.
```haskell
data Price = Price Integer deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)

data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited
    deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
              | Plane Airline
    deriving (Eq, Show)
```

Question: What is the type of `Car`?

My first guess was it was `Vehicle` but that isn't accurate.
```haskell
GHCi> :t Car
Car :: Manufacturer -> Price -> Vehicle
```

Yep. Car is a data constructor that takes two arguments and returns a `Vehicle`.

## Another Interesting Example

Here is the type definition for `[]`:
```haskell
data [] a = [] | a : [a]
```

According to Haskell First Principles p. 300, "Whereas the list datatype as a whole is a sum type, as we can tell from the `|` in the definition, the second data constructor, `:`, is a product, because it takes two arguments."

Well that is pretty interesting as it shows:
1. the name of a data constructor does not have to be first (i.e., `:` is between `a` and `[a]`).
2. the name of a data constructor can be a symbol like `:`. (I have seen `:` called a data constructor but didn't get it till now.)
3. Not sure I completely get this yet but seems: A overall data type can be a sum type but still have a data constructor that is a product type.

## Observation

It appears two types of the same types with the same values are equal to each other. (hffp p.399)
```haskell
GHCi> data Ex = Ex Int String deriving (Eq, Show)
GHCi> ex1 = Ex 1 "hi"
GHCi> ex2 = Ex 1 "hi"
GHCi> ex1 == ex2
True
```

## Observation

A data constructor can have an argument even if the type constructor does not. (hffp p.400)
```haskell
data MyType = MyVal Int deriving (Eq, Show)
```

