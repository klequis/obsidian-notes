# ?: Why do the type for `rotateMany` and `rm1` differ?

## Conclusions
- It seems that a function that type inference flags as having an ambiguous type can be clarified such that there is no longer an ambiguous type by having a function that calls it.
- Some of this isn't worth spending the time on now

## Analysis

**(A1)** If I have
```haskell
rotateMany = foldl (flip rotate)
```

**(A2)** But it isn't being called, HSL gives its signature as
![[Pasted image 20221007103034.png]]
        And gives an "ambiguous type" error.

**(A3)** However, if I call `rotateMany`, HLS is able to resolve the type
![[Pasted image 20221007103241.png]]

**(B1)** There is a different situation if explicit arguments are supplied to `rotateMany`. If called or not called makes no difference

**(B2)** If called
![[Pasted image 20221007103619.png]]

**(B3)** If not called
![[Pasted image 20221007103849.png]]

**(B4)** However, I can explicitly use the signature from **(A3)** and all is well
![[Pasted image 20221007104524.png]]

**Conclusion**
1. Type inference can use a call to a function to figure out the type of a function.

**Questions**
1. Are type inferences from HLS always the same as what would be product by type inference when not using HLS in VS Code?
2. What does the type signature in **(A3)** differ from **(B2)**? Since the function is called should't it figure out the specific type as it did in **(A3)**?
    - `rotateMany :: Direction -> [Turn] -> Direction`
      VS
    - `rotateMany Foldable t => Direction -> t Turn -> Direction`

# ?: Why do the type for `rotateMany` and `rm1` differ?


```haskell

rotateMany :: Direction -> [Turn] -> Direction
rotateMany = foldl (flip rotate) -- foldl (+)    42        [1,2,3,4]
                                 -- foldl rotate Direction [Turn]

rm1 direction turns = foldl (flip rotate) direction turns
```

Compare the types
```haskell
rotateMany ::               Direction -> [Turn] -> Direction
rm1        :: Foldable t => Direction -> t Turn -> Direction
```

- [Turn] is a `Foldable`


## I'm not going to ask this question because:

Although HLS suggests the below
```haskell
rm1 :: Foldable t => Direction -> t Turn -> Direction
rm1 direction turns = foldl (flip rotate) direction turns
```

The signature for `rotateMany` can be substituted in.

```haskell
rotateMany :: Direction -> [Turn] -> Direction
rm1 direction turns = foldl (flip rotate) direction turns
```

Therefore, I assume it doesn't matter.