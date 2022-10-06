- Interesting: `Bool` and `Int` are not type classes, they are data declarations.
- The `Bool` data declaration has instances of type classes
	- Looking at `data Bool` in Prelude shows that `Bool` has a number of instances including one for `Eq`.
		- This covers "a type has an instance of"
			- Are instances "always of" "type classes" or can they be an instance of something else.
- "All Fractional numbers implement the Num type class"


Let's look at `Ord` & `Eq`

- All members of `Ord` must be members of `Eq`

---

There are 3 Bool.hs files

- libraries/base/Data/Type/Bool.hs
- libraries/base/Data/Bool.hs
- compiler/GHC/Data/Bool.hs

[https://wiki.haskell.org/OOP_vs_type_classes#Type_classes_is_a_sort_of_templates.2C_not_classes](https://wiki.haskell.org/OOP_vs_type_classes#Type_classes_is_a_sort_of_templates.2C_not_classes)

[https://wiki.haskell.org/OOP_vs_type_classes](https://wiki.haskell.org/OOP_vs_type_classes)

## [Type classes vs classes](https://wiki.haskell.org/OOP_vs_type_classes#Type_classes_are_like_interfaces.2Fabstract_classes.2C_not_classes_itself)

**Type classes**
- no inheritance
- no data fields
- in terms of C# they are like generic interfaces
- do not implement methods
	- they guarantee that the actual types that instantiate the type class will implement specific methods
	- they provide the type parameters, e.g.:
		- `(==) :: a -> a -> Bool`
	- BUT, they can decide to provide default implementations
