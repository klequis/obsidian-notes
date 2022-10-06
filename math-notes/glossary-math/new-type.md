# newtype

---

## newtype
*Haskell Programming from First Principles, ch 15, sec 15.6 p. 579*

**data vs. newtype**

```haskell
data Server = Server String
newtype Server' = Server' String
```

- They are mostly the same
- newtype
	- constrains the datatype to haveing a single unary data constructor
	- guarantees no additional runtime overhead in wrapping the original type.

*Why/when use `newtype`*.

1. Signal intent
	1. only a wrapper for underlying type
	2. can not grow into sum or product type
		1. a normal type can
2. Improve type safety
3. Add different type class instances to a type that is other wise unchanged representationally, such as `Sum` and `Product`.

---

