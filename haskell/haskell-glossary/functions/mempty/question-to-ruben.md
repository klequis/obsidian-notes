I don't really get that yet :(. As I think through you post I'm getting more questions.

**1. What is the `_` holding a place for? The function?**

The monoid for `[a]` has
```haskell
mempty = []
```

It doesn't have the `_` (whatever placeholder) like monoid for `(a -> b)`.
```haskell
mempty _ = mempty
```

Perhaps like:
```haskell
> mempty (+1)
()
```


**2. Since the monoid for `[a]` is `[]` why does this return `()`? **

```haskell
> mempty [3]
()
```
Oh, I see that
```haskell
> mempty [3] == []
True
```
So not sure what is happening there.

**3.  If `mempty _ = mempty` doesn't there need to be a definition for `mempty` on the RHS**
I realize you said it is using `b` but I didn't understand that.

I don't really understand "unit" `()`. Does my lack of clarity on that relate to the original question?