
```haskell
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (Eq, Ord, Show, Bounded, Enum)
```

- Data constructors for enumerated types numst be nullary
- At a minimum you need to derive `Show` and `Enum`
- Adding Bounded gives you minBound & maxBound
- Add `Ord` gives you comparison operators such as `>`, `<`. But when you add `Ord` you must also add `Eq`.
- Enumeration is behind the arithmetic sequences
- Class Enum has a set of operations that underlie the syntactic sugar of arithmetic sequences. I.e.,:
	- `[x..y]` where
		- `x` & `y` are data constuctors in a type with an instance of `Enum`.
		- `x` & `y` can be
			- numbers
			- char
			- user-defined type such as `[Monday ... Sunday]`

**`toEnum`**

```haskell
toEnum 3::Day
-- Thursday
```

**`fromEnum`**

```haskell
fromEnum Tuesday
-- 1
```

## 8.3 The Read and Show classes

> Note: Haskell First Priniples book says you should never use `Read`. It is likely that the current book is quite old and things have changed.


- Instances of class `Show` are those types that can be converted to character strings.
- `Read` provides operations for parsing character strings to obtain the values they may represent.

		I skipped this section as I suspect it is outdated and/or there are better sources for this.

