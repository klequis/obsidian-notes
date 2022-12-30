**Polymorphic type variables** give us the ability to implement expressions that can accept arguments and return results of different types without having to write variations on the same expression for each type.

Type signatures may have three kinds of types: concrete, constrained polymorphic or parametrically polymorphic.

Haskell has *two types of polymorphism*:
- **parametric polymorphism**
      - refers to type variables, or parameters, that are fully polymorphic. (no type class constraints).
- **constrained polymorphism**
      - Often called ***ad-hoc polymorphism***
      - Also know as ***overloading***
      - Is implemented with *type classes* ("type class constraints")
      - 
n