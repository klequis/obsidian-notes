#higher-kinded-type

> [!WARNING]
> - in HPFFP it says: The kind `* -> * -> *` is a #higher-kinded-type  
> - But in the article [] it says: #higher-kinded-type's are types with kind signatures that **have parenthesis somewhere on the left side**

## [Kinds and Higher-Kinded Types in Haskell](https://serokell.io/blog/kinds-and-hkts-in-haskell)

Higher-kinded types are types with kind signatures that have parenthesis somewhere on the left side, like this: 
```haskell
`(* -> *) -> * -> *`.
```
This means that they are types that take a type like `Maybe` as an argument. In other words, they abstract over polymorphic types.

### Example: A type for any collection

> [!IMPORTANT]
> This is "A type for any collection". It is NOT "A collection for any type"

```haskell
data Collection f a = Collection (f a) deriving (Show)

> :k Collection
Collection :: (* -> *) -> * -> * 
```

YEP - it is higher-kinded bec of the `(* -> *)`

Usage:
```haskell
a :: Collection [] Int
a = Collection [1,2,3]

b :: Collection [] String
b = Collection ["call", "me", "ishmael"]

c :: Collection Maybe String
c = Collection (Just "whale")
```

## [Haskell Programming from first principles ](https://haskellbook.com/)

p.435


 

## Important Points

- #kinds are the types of #type-constructor 's
	- Example:  `Maybe` has a kind, `Just` does not.


## References
- [Kinds and Higher-Kinded Types in Haskell](https://serokell.io/blog/kinds-and-hkts-in-haskell)
- [Haskell Programming from first principles ](https://haskellbook.com/)
- [Haskell's kind system - a primer](https://diogocastro.com/blog/2018/10/17/haskells-kind-system-a-primer/)
	- Not used in this subject but a good article on various advanced kinds




---

#higher-kinded-type's are type constructors, i.e., types that take more types as arguments. 

```haskell
fmap (a -> b) -> f a -> f b
```

`f` is a #higher-kinded-type because it takes an argument (E.g., `f a`  and `f b`)