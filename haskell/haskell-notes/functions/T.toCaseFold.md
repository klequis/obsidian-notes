**[T.toCaseFold](https://hackage.haskell.org/package/text-2.0.1/docs/Data-Text.html#v:toCaseFold)**

- Is mainly useful for performing caseless (also known as case insensitive) string comparisons.
- Converts the whole Text value to the folded case and does that significantly faster than mapping with `toLower` over every character.
- Respects unicode

## Example
```haskell

txt :: T.Text

txt = "This is some text, or you are led to believe so, that can't be seen. All you can do is hope."

> T.toCaseFold txt
"this is some text, or you are led to believe so, that can't be seen. all you can do is hope."
```

