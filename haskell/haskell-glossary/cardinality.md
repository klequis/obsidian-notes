# #cardinality

## With types

The cardinality of a datatype is the number of possible values it defines. That number can range from 0 to infinity.

- Nullary data constructors have a cardinality of 1
- Unary data constructors have the same cardinality as the types they contain.
  - `Int 8` has a cardinality of 256
  - `data Goats = Goats Int8` has the same cardinality as `Int8` (i.e., 256).