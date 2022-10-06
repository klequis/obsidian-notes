# #polymorphic-constant

> [!WARNING] As far as I know thus far.

- Applies to numbers
- 3 is not a variable
- But it is of type `Num a => a` until determined otherwise.
- It will have to resolve into a concrete type at some point in order to evaluate.

3 is only constrained by `Num` and therefore can be any type of number.
```haskell
> :t 3
3 :: Num p => p
```

In the context of an equation that has 3 & 4.0, 3 becomes fractional.
```haskell
> a = 3 + 4.0
> :t a
a :: Fractional a => a
```

Here the variable `a` is set to 4 and we specify that it is a `Float`. When 3 is used in an equation it becomes the concrete type `Float`.
```haskell
> a = 4 :: Float
> b = a + 3
> :t b
b :: Float
```
