# Data.Text

## Imports & Pragmas
```haskell
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
```

## `lines`
**description**: break on "\\n"
**input**: `"Line One\nLine Two \nLine Three"`
**output**: `["Line One","Line Two ","Line Three"]`
```haskell
someLines :: T.Text
someLines = "Line One\nLine Two \nLine Three"

linesEx :: [T.Text]
linesEx = T.lines someLines
```

## `splitOn`
**description**:  split on any delimiter
**input**: `"one.two.three"`
**output**: `["one","two","three"]`
```haskell
splitText :: T.Text
splitText = "one.two.three"

splitEx :: [T.Text]
splitEx = T.splitOn "." splitText
```

## `words`
**description**: Brake Text into list of words breaking on white space.
**input**: `"Word1 Word2 Word3"`
**output**: `["Word1","Word2","Word3"]`
```haskell
wordsText :: T.Text
wordsText = "Word1 Word2 Word3"

wordsEx :: [T.Text]
wordsEx = T.words wordsText
```

## `b`
**fnName**: `unwords`
**description**: Join text separated by a space.
**input**: `["Word1","Word2","Word3"]`
**output**: `"Word1 Word2 Word3"`
```haskell
unWordsList :: [T.Text]
unWordsList = ["Word1","Word2","Word3"]

unWordsEx :: T.Text
unWordsEx = T.unwords unWordsList
```

## `ulines`
**description**: Joins lines, after appending a terminating newline to each.
**input**: `["Line One","Line Two ","Line Three"]`
**output**: `"Line One\nLine Two \nLine Three\n"`
```haskell
listOfLines :: [T.Text]
listOfLines = ["Line One","Line Two ","Line Three"]

unlinesEx :: T.Text
unlinesEx = T.unlines listOfLines
```

## `intercalate`
**description**: Concatenates the list after interspersing the first argument between each element of the list.
**input**: `"NI!" ["We", "seek", "the", "Holy", "Grail"]`
**output**: "We-seek-the-Holy-Grail"
```haskell
`T.intercalate "-" ["We", "seek", "the", "Holy", "Grail"]`
```

**description**: 
**input**: 
**output**: 

**description**: 
**input**: 
**output**: 

---
---

**description**: 
**input**: 
**output**: 


```

## `b`
**fnName**: 
**description**: 
**input**: 
**output**: 

## `b`
**fnName**: 
**description**: 
**input**: 
**output**: 

