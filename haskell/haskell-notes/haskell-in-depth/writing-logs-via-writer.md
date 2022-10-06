
**Goal:**
- Write a log for steps in a computation
	- log ever step or certain steps

- `>>` - Sequentially compose two actions, **discarding any value produced by the first**, like sequencing operators (such as the semicolon) in imperative languages.
- `>>=` - As used in the `Writer` monad is implemented as **appending logs to each other**.
	- if you have 2 operations `tell [1]` and `tell [2]` then the resulting log is `[1,2]`.
- `<$>` - An infix synonym for `fmap`
	- E.g., 
	```haskell
	> (*2) <$> [1,2,3]
	[2,4,6]

	-- is the same as
	> (*2) `fmap` [1,2,3]
	[2,4,6]
	```

## writer
```haskell
writer :: (a, w) -> m a
```
`a` - actual result
`w` - accumulated result

By providing such a pair, we can 
- construct a computation using the writer function 
- or extract it from a computation with the listen
function.

## Functions to run a `Writer` compuation

### runWriter

`runWriter :: Writer w a -> (a, w)`

- Returns a result (`a`) and an accumulated value (`w`)

### executeWriter

`execWriter :: Writer w a -> w`

- Returns the accumulated value (i.e., the log) (`w`) only.

### mapWriter

`mapWriter :: ((a, w) -> (b, w')) -> Writer w a -> Writer w' b`

Modifies a `Writer` computation by converting both a result and a log to other types.

## other

- other function ssuch as `pass`, `listens` & `censor` provide utilities for postprocessing log manipulation.