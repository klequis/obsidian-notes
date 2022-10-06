# Algebra

*Haskell Programming from First Priciples, p. 573, sec 15.2*

**An algebra**
- An algebra refers to some operations and the set they operate over. Here again, we care less about the particulars of the values or data we’re working with and more about the general rules of their use.

**What we mean by an algebra**
- The study of mathematical symbols and the rules governing their manipulation. 
- It is differentiated from arithmetic by its use of abstractions such as variables. 
- By the use of variables, we’re saying 
	- we don’t care much what value will be put into that slot. 
	- We care about the rules of how to manipulate this thing without reference to its particular value.

**In Haskell,** 
- Algebras can be implemented with *type classes*. 
- The *type class* defines the *set of operations*. 
- When we talk about operations over a set
	- *the set is the type* the operations are for. 
- The instance defines how each operation will perform for a given type or set. 

**Monoid** is an algebra.

---

