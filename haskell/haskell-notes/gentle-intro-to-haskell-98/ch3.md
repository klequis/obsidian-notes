## 3 Functions

> Function application has higher precedence than any infix operator.

## 3.1 Lambda Abstractions

```haskell
inc :: Num a => a -> a
inc x = x+1
```

```haskell
-- inc
\x -> x+1

-- add
\x -> \y -> x+y

-- add shorthand
\x y -> x+y
```

## 3.2 Infix Operators
- In Haskell, the partial application of an infix operator is called a [section](https://github.com/klequis/haskell-notebook/issues/70).
- Infix operators are just functions
- Therefore you can partially apply them

```haskell
(x+) = \y -> x+y

(+y) = \x -> x+y

(+) = \x y -> x+y
```

The last form above essentially coerces an infix operator into an equivalent functional value, and is handy when passing an infix operator as an argument to a function.

## 3.3 Functions are Non-strict

`bot = bot`

`bot` is a non-terminating expression denoted by $\perp$ (read "bottom"). Expressions that result in a run-time (non-recoverable) error, such as I/0, also have this value.

**A function is said to be *strict*** if when applied to a nonterminating expression, it also fails to terminate.

Consider

```haskell
const1 x = 1
const1 bot -- 1
```

shows that `constl` is non-strict which is alwo called *lazy* functions.

> Error and nonterminating values are semantically the same in Haskell (? what does that mean.)

Another way of explaining non-strict functions is that:
- Haskell computes using _definitions_ rather than the _assignments_ found in traditional languages.

```haskell
v = 1/0
```

should be read as "define `v` as 1/0" instead of "compute 1/0 and store the result in `v`".

## 3.4 "Infinite" Data Structures

- Data constructors are also  *non-strict* because they are just a special kind of function.
- This permits the definition of (consceptually) infinite data structors.





