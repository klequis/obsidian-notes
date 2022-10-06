# **[T.DropAround](https://hackage.haskell.org/package/text-2.0.1/docs/Data-Text.html#v:dropAround)**


- `dropAround p t` Returns the substring remaining after dropping characters that satisfy the predicate `p` from both the beginning and end of `t`.

## Example
```haskell
txt :: T.Text
txt = "This, has; some. punctuation!"

> map (T.dropAround $ not . isLetter) $ T.words txt
["This","has","some","punctuation"]

```