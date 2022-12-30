```haskell
data Tree a = Leaf a | Node (Tree a) (Tree a)
```

1. Declare the data type itself and how many type parameters it takes
    - This information appears right after the data keyword, and each subsequent type parameter starts with a lowercase letter. 

2. *Constructors*
    - `Tree` has two, `Leaf` & `Node`
    - They are functions that are used to build new elements of the data type.

3. `Leaf a`
    - Holds one piece of information of the type given as the argument to `Tree`
        - So if you have `Tree Int` then `a` is an `Int` and must be so in both `Leaf` and `Node`

4. `Node (Tree a) (Tree a)`
    - Represents an *internal node* (is that a Haskell term?)
    - It is a *recursive* constructor since it holds two other trees inside of it.

## Count the number of leaves in a binary tree (p. 14)

```haskell
numberOfLeaves :: Tree a -> Integer
numberOfLeaves (Leaf _) = 1
numberOfLeaves (Node 1 r) = numberOfLeaves 1 + numberOfLeaves r
```