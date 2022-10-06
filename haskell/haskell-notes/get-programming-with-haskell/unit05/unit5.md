# Working with a type in a context

> `Functor`, `Applicative` and `Monad`
> Each builds on the to allow you to work in contexts such as `IO`


Context are things like `Maybe` and `IO`.
- OK, you should have known that. `Maybe` is for a value that maybe missing and `IO` is for stuff that has interacted with I/O and covers a number of possibilities (e.g., errors).

## Functor

- Transforming types that are in context with a function that is not in context.

## Applicative

### Solves 1
- Solves: the first part of a function is in context but its result is not.

*Looks to me like*
- a function 
- takes a value that is in context
- outputs a value that is not in context
- but you need to put it back into context

### Solves 2

- Solves: a function that might be missing
- Therefore the function is wrapped in a context such as `Maybe`

## Monad

- the argument to a function isn't in context
- but the result is