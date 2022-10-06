# #polymorphism

## Introduction

> [!INFO] Types in Haskell
> Generally speaking, in Haskell, type signatures may have three kinds of types:
> - concrete
> - constrained polymorphic
> - parametrically polymorphic

Wikipedia defines [Polymorphism (computer science)](https://en.wikipedia.org/wiki/Polymorphism_(computer_science)) as:
 In programming language theory and type theory, polymorphism is the provision of a single interface to entities of different types or the use of a single symbol to represent multiple different types. The concept is borrowed from a principle in biology where an organism or species can have many different forms or stages.

## #polymorphic

**polymorphic** is derived from Greek and means "made of many forms".

## #monomorphic

**monomorphic** is derived from Greek and means "made of one form".

## Polymorphism in Haskell

Haskell has both **constrained polymorphism** and **parametric polymorphism**

**Constrained polymorphism** is often called **ad-hoc polymorphism**. Constrained polymorphic types
-  are constrained by type classes ("type constraints") that decrease the number of concrete types they can be.
- increase what you can do with them compared to parametric polymorphic types by defining and bringing into scope a set of specific operations.
- Is similar to [subtyping](https://en.wikipedia.org/wiki/Subtyping) in other languages.

**Parametric polymorphism** refers to type variables, or parameters, that are fully polymorphic. <span style="color:#CC9900">(not sure what is meant by "parameters" here. maybe same as type variables.)</span>. Parametric polymorphic types
- are *unconstrained* in that they can become any type.
- but have no methods/functions associated with them.

> [!INFO]
> A type variable `a` is parametrically polymorphic (unconstrained) and therefor has no methods (functions) associated with it, because functions require type information.
> 
> If you put a type constraint on `a` as in `Num a => a`, `a` will now have some the methods from the `Num` class, such as `(+)` & `(*)`, which you can find [here](https://hackage.haskell.org/package/base-4.16.1.0/docs/Prelude.html#t:Num).
> 

A **type variable** represents a set of possible types. When there is no type class constraint, the set of possible types a type variable can represent is unlimited.
