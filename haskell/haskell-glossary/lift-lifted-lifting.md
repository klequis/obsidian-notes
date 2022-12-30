#lift #lifted #lifting

## (1)
... `fmap` transforms a “normal” function `(g :: a -> b)` into one which operates over containers/contexts `(fmap g :: f a -> f b)`. This transformation is often referred to as a **lift**; `fmap` “lifts” a function from the “normal world” into the “f world”. (source: [Typeclassopedia](https://wiki.haskell.org/Typeclassopedia#Colophon))


## (2)
> [!WARNING] (Re below) Just reiterating what is in the book
> HFF p. 466

- kind * is the kind of all standard lifted types
- types of kind # are unlifted
* Any data type you create yourself is lifted.
* A lifted type is any type that can be inhabited by *bottom*
* An unlifted type is any types that *cannot* be inhabited by bottom.
*  Lifted types are represented by a pointer and include most of the datatypes we’ve seen and most that you’re likely to encounter and use.

Types of type # are often native machine types and raw pointers.

**Newtypes**

Newtypes are a special case in that they are kind *, but they are unlifted, because their representation is identical to that of the type they contain, so a newtype itself is not creating any new pointer beyond that of the type it contains. 

That fact means that the newtype itself cannot be inhabited by bottom—only the thing it contains can be—so newtypes are unlifted.

# [Lifting, wiki.haskell](https://wiki.haskell.org/Lifting)

Using:
```haskell
data Pair a = Pair a a deriving Show
instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)
```

`fmap` is lifting. It takes a function `(a -> b)` and allows it to tranform a value that is in a context. You could say "the function was lifted into the context". In the `Functor Pair` instance above
- `f` an `(a -> b)`
- is lifted into a `Pair` context

So `lift` in this case == `fmap`
```haskell
lift :: (a -> b) -> Pair a -> Pair b
lift = fmap
```


However, **a `Functor` can only lift functions of exactly one valiable**. You can lift this with `fmap`
```haskell
\(x, _) -> (x, 0)
```
