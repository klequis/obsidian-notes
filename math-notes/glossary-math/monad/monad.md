#monad 
A Monad is a design pattern for types and "a monad" is a type that uses that pattern.

Traditional name for the constructor function is "unit".

In Haskell, the unit function for Monad is called "return".

Traditional name for "function application helper" is "bind".

In Haskell, to apply a function f to an instance of a monad m, you would say `m >>= f`.

- `m >>= f` means "bind operation `f` onto the end of workflow `m` and give me the resulting new workflow".
- in other words: the bind operation takes a workflow and a function and gives you back a new workflow that feeds the result of the original workflow into the new function when the workkflow is executed.
	- "bind" does not execute

> The *nullable monad in c#* is the *maybe monad*  in other languages.

## Rules of Monad

- The first rule of the monad pattern is that there’s always an easy way to go from a value of the “underlying” type to a value of the “wrapped” type.
- The second rule of the monad pattern is that adding one to a wrapped int somehow produces another wrapped int, and does so in a way that preserves the desired “amplification”.
- Rule three: there is a helper method

```cs
static M<R> ApplySpecialFunction<A, R>(
  M<A> wrapped, 
  Func<A, M<R>> function)
```

Author goes on to say that rule two is implied by rule 3 so is redundant. my interpretation:

1. There is always a way from a value of type `x` to a wrapped value of type `x`
2. The monad will flatten values so that you don't get back nest wrapped values such as `Nullable<Int>Nullable<int>3'
3. they must be composable


I have become lost in this but here is my sum of what is a monad per this article so far.

1. you can take a value of `x` and get a wrapped value of type `x`.
2. The monad will flatten values so you don't get values that are wrapped multiple times (i.e., nested).
3. They must be composable.


## The autor's rule summary

1. Applying the construction function to a given instance of the monad produces a logically identical instance of the monad.
2. Applying a function to the result of the construction function on a value, and applying that function to the value directly, produces two logically identical instances of the monad. 
3. Applying to a value a first function followed by applying to the result a second function, and applying to the original value a third function that is the composition of the first and second functions, produces two logically identical instances of the monad


#2 could be described as:

```haskell
m a == m (m a)
```

That is likely sudo code but it says: *a monad of `a`* is the same as *a monad of a monad of `a`*.

#3 says

```haskell
f a -> g -> result
```

Again sudo code but says: 
- the output of `f a` is passed to `g`
- the output of `g(faOutput)` is the `result`

And I'm sure you could interprete this as

```haskell
f a . g a = f(g(a))
```

Adn I finally totall get that on an intuitive level !!!!

