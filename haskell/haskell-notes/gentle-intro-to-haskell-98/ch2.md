## 2.0 Values, Types & Other Goodies

"Haskell is a typeful programming language", Luca Cardelli


- Haskell has a *static type system* that defines the formal relationship between types and values and insures Haskell programs are *type safe*.
- All computations are done via the evaluation of _expressions_ to yield *values*.
- Every *value* has an associated *type*.
	- *Types* are sort of sets of *values*.
- Expression examples
	- `5`
	- `'a'`
	- `\x -> x+1`

***Expressions*             denote *values***
***Type expressions* denote *type values***

![[Pasted image 20220306071335.png]]

- Examples type expressions
	- `Integer`
	- `Char`
	- `Integer -> Integer`
	- `[Integer]`
	- `(Char,Integer)`

***Values* are first class, *types* are not.**

- *Types* describe *values* and *typing*.
- *Typing* is the association of a *value* with its *type*.
- Example
	- `5 :: Integer`
	- `5` "has type" `Integer`

**Functions**

- *Functions* are normally defined by a series of equations.
- An equation is a _declaration_.
- A _type signature_ is a _declaration_.

## 2.1 Polymorphic Types

*Type variables*
- Are uncapitalized.
- Are [universally quantified](https://en.wikipedia.org/wiki/Universal_quantification) types
	- Haskell only has universally quantified types

- *Polymorphic* types are types that are universally quantified in some way over all types. 
- They essentially describe families of types.
- E.g.,
	- `(forall a)[a]` is the family of types: "for every type `a`, the type of list of `a`."
		- Can be list of integers, chars, etc.
		- But all members of the list must be the same type.
- Lists are polymorphic - they can contain any type.
- The `length` function is polymorphic - it can be applied to a list of any type.

Haskell's type system posesses two import properties:
1. Every well-typed expression is guaranteed to have a unique principal type.
	- The principal type of `head` is `[a]->a`. `
	- `[b]->a`, `a->a` & `a` are correct types but too general.
	- `[Integer]->Integer` is too specific.
2. The principal type can be inferred automatically

## 2.2 User-Defined Types
`data Bool  = False | True`

`data Color = Red | Green | Blue | Indigo | Violet`

> nullary: no parameters

- Both `Bool` and `Color` 
	- Are (nullary) *type constructors*
	- Are enumeraged types (a finite number of nullary data constructors).
- `True`, `False` and the list of colors are (nullary) *data constructors*.

- `[]` is a type constructor because applying `[]` to any type `t` yields a new type `[t]`.
- `->` is a type constructor because given two types `t` and `u`, `t->u` is the type of functions mapping elements of type `t` to elements of thpe `u`.

a) Applying a *data* constructor to yield a *value* 
    - happens at run-time and is how things are computed in Haskell.
     VS. 
b) Applying a *type* constructor to yield a *type*.
	-  happens at compile-time and is part of tye type system's process of ensuring type safety.

Type constructors and data constructors are in separate namespaces and therefore the name of a type constructor can also be a data constructor as in `data Point a = Point a a`.

## 2.4.1 List Comprehensions and Arithmetic Sequences

A  *list comprehensions*
`[ f x | x <- xs ]`

### Generators
As in set notation ` <- ` is called a generator and more than one is allowed
`[ (x,y) | x <- xs, y <- ys ]`

If `xs` is `[1,2]` & `xy` is `[3,4]` the result is the cartesian product `[(1,3),(1,4),(2,3),(2,4)]`.

### Guards

- *Guards* are boolean expressions.
- *Guards* place constaints on elements generated.

`evenElem xs = [ x | x <- xs, even x]`

## Arithmetic Sequences

`[1..10]` results in `[1,2,3,4,5,6,7,8,9,10]`



