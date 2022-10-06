## Functor
#functor

In mathematics/category theory
- A _functor_ is a mapping between categories.
- The set must have an identity
	- $F(id_x) = id_{f(x)}$ 
-  and be composable
	- $F(g \cdot f) = F(g) \cdot F(f)$ 

In Haskell the `Functor` class implements the fnctor laws

```haskell
fmap id      = id              -- identity
fmap (f . g) = fmap f . fmap g -- composition
```

---

1. It seems to me that *magma* is the root of the other terms I'm learning in functional programming.
2. See table below for traits of different *group-like structures*

![[algebraic-structures-magma-to-group.png]]

**source:** [Wikipedia](https://en.wikipedia.org/wiki/Magma_(algebra))

## Magma

A magma is 
- a set set $M$
- matched with an operation $\bullet$
- that sens any two elements $a, b \in M$ 
- to another element $a \bullet b$.
- Symbol $\bullet$ is a generall placeholder for the operation.

The *magma* or *closure axiom*:
- For all $a, b$ in $M$, 
- the result of operation $a \bullet b$ is also in $M$.
- In mathematical notation it is:
	- $a, b \in M \Longrightarrow a \bullet b \in M$
	- 



## Semigroup

#semigroup

Nothing here yet.

---

## Monoid
#monoid

---
**source**
- [Monoid (category theory)](https://en.wikipedia.org/wiki/Monoid_(category_theory))
- [Monoid](https://en.wikipedia.org/wiki/Monoid)

**In abstract algebra**
A monoid is:
- a set equiped with
	- an associative binary operation
	- and an identity element
- A monoid is a semigroup with identity

**In category theory**
A monoid is:
- An object with M together with two morphisms
	- $u: M \otimes M\to M$ called *muliplication*
	- $n: I \to M$ called *unit* (where $I$ is the unit element)

A **Monoid** is a set
- with a binary operation
- that is associative
- and has a special *unit* type element


---

*Totality* is also known as the *closure axiom*.
- In mathematics, a subset of a given set is **closed** under an operation of the larger set if performing that operation on members of the subset always produces a member of that subset.
- Example 1
	- The positive integers are closed under addition but not under subtraction. I.e., $1 - 2 = -1$ which is not a positive integer.
- Example 2
	- The set of even integers is closed under addition, but the set of odd integers is not.
- When a set $S$ is not closed under some operations, the smallest set (if it exists) contain $S$ that is closed is called a **closure** of $S$.

|                    | Totality | Associativity | Identity | Invertibility | Commutativity |
|--------------------|:--------:|:-------------:|:--------:|:-------------:|:-------------:|
| Semigroupoid       |          |    $\checkmark$   |          |               |               |
| Small category     |          |    $\checkmark$   | $\checkmark$ |               |               |
| Groupoid           |          |    $\checkmark$   | $\checkmark$ |    $\checkmark$   |               |
| Magma              | $\checkmark$ |               |          |               |               |
| Quasigroup         | $\checkmark$ |               |          |    $\checkmark$   |               |
| Unital magma       | $\checkmark$ |               | $\checkmark$ |               |               |
| Semigroup          | $\checkmark$ |    $\checkmark$   |          |               |               |
| Loop               | $\checkmark$ |               | $\checkmark$ |    $\checkmark$   |               |
| Group or Empty     | $\checkmark$ |    $\checkmark$   |          |    $\checkmark$   |               |
| Monoid             | $\checkmark$ |    $\checkmark$   | $\checkmark$ |               |               |
| Commutative monoid | $\checkmark$ |    $\checkmark$   | $\checkmark$ |               |    $\checkmark$   |
| Group              | $\checkmark$ |    $\checkmark$   | $\checkmark$ |    $\checkmark$   |               |
| Abelian group      | $\checkmark$ |    $\checkmark$   | $\checkmark$ |    $\checkmark$   |    $\checkmark$   |