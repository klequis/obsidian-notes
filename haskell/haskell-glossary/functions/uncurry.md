# uncurry

#uncurry

`uncurry :: (a -> b -> c) -> (a, b) -> c`

`uncurry (+) (1,2)`
- takes
	- a function that takes `a` and `b` and returns `c` (i.e., a #binary-function )
	- a pair/tuple
- returns
	- `c`

In the above example, `uncurry` adds $1$ & $2$ to produce $3$.
