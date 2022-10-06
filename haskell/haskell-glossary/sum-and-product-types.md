# Sum & Product Types

# #sum-type
# #product-type

- **Algebraic data types:** are any types that can be made by combining other types.
- You can combine types with an _and_ or with an _or_.
- **Product types:** are types made by combining two or more other types with an _and_.
- **Sum types**: are types made by combining other types with an _or_.

Most languages such as Java, C++, etc only enable you to create types with an _and_. Example:

```java
public class Book extends StoreItem {
	Author author;
	String isbn;
}
```

With Haskell you can use an _or_:

```haskell

type FirstName = String
type LastName = String
type MiddleName = String
data Name = Name FirstName LastName
    | NameWithMiddle FirstName MiddleName LastName
    | TwoInitialsWithLast Char Char LastName


data Author = Author Name
data Creator = AuthorCreator Author | ArtistCreator Artist
data Artist = Person Name | Band String

```

## Cardinality of types
*(hffp p. 401)*
- The cardinality of a datatype is the number of possible values it defines.
- Cardinality can range from 0 to infinity.
- You can calculate the cardinality of a type based solely on its definition.
- A types cardinality tells you how many possible implementations there are of a function of a given type signature.11

#### Cardinality of sum types
*hffp p. 411*
```haskell
data QuantumBool = QuantumTrue
  | QuantumFalse
  | QuantumBoth
  deriving (Eq, Show)
-- yes, it is a silly type
```

The cardinality of `QuantumBool` is 3. (It has 3 data constructors and I'm assuming this is the cardinality of a sum type is always the number of data constructors.)

#### Cardinality of product types
```haskell
data TwoQs =
  MkTwoQs QuantumBool QuantumBool
  deriving (Eq, Show)
```

The number of potential values that can manifest in this type is the cardinality of one of its type arguments times the cardinality of the other. So, what is the cardinality of `TwoQs`? (well 3 x 3 = 9).

#### Cardinality of unary constructors
Datatypes that only contain a unary constructor always have the
same cardinality as the type they contain.
```haskell
data Goats = Goats Int deriving (Eq, Show)
```
`Goats Int` will have the same cardinality as `Int`

# Quotes on product and sum types
> **Collecting info from multiple sources in attempt to get complete definitions of sum and product types.**

### Get Programming
#### Product Type

#### Sum Type


### First Principles
#### Product Type
**p.107**
(Speaking of tuples): A product type represents a logical conjunction: you must supply both arguments to construct a value.

**p. 269**
With respect to products, pattern matching gives you the means for destructuring and exposing the contents of products, binding one or more values contained therein to names.

**p. 344**
**a.** In type theory, a product type is a type made of a set of types *compounded over each other* (whatever that means see **c**). 
**b.** In Haskell, we represent products using tuples or data constructors *with more than one argument*.
**c.** The “compounding” is from each type argument to the data constructor representing a value that coexists with all the other
values simultaneously.
**d.** Products of types represent a conjunction, “and,” of those types. If you have a product of Bool and Int, your terms will *each contain a Bool and an Int value*.

**p. 388**
The data constructors in product types have more than one parameter.

**p. 412**
Records in Haskell are product types with additional syntax to provide convenient accessors to fields within a record.

**p. 416**
Products distribute over sums. Just as we would do with the expression a * (b + c).
- Is this referring to a "distributive" property in math?

#### Sum Type

**p. 89**
The pipe | indicates a sum type or logical disjunction: or.

**p. 269**
Pattern matching is a syntactic way of deconstructing product and sum types to get at their inhabitants.
With sums, pattern matching lets you discriminate which inhabitant of a sum you mean to handle in that match.

**p. 344**

In type theory, a sum type of two types is a type whose terms are terms in either type, but not simultaneously. In Haskell, sum types are represented using the pipe, |, in a datatype definition. Sums of types represent a disjunction, “or,” of those types. If you have a sum of Bool and Int, your *terms will be either a Bool value or an Int value*.

**p. 388**
The pipe denotes what we call a sum type, *a type that has more than one constructor inhabiting it*.

**p. 409**
Sum types are a way of expressing alternative possibilities within a single datatype.



### newtype
- Can only ever have a single unary data constructor.
- Like all unary constructors, the cardinality of `newtype` is the same as the type it contains.
- A `newtype` cannot be a product type, sum type, or contain nullary constructors.
- The difference between newtype and the type it contains is gone by the time the compiler generates the code.
- Like a type synonym a `newtype` that contains an `Int` is an `Int`.
- 
#### newtype advantages
- `newtype` has no runtime overhead, as it reuses the representation of the type it contains. It can do this, because it’s not allowed to be a record (product type) or tagged union (sum type). 
- you cannot declare class instances for type alias's but you can for `newtype`.


>[!QUESTION] Argument to type constructor
> ```haskell
> data Chicken a = Chicken a -- (A)
> data Horse = Horse a           -- (B)
>  ```
>  When does the type constructor need an argument and what does it change?


