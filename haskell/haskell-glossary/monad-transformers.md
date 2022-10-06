# #monad-transformers

> This may not belong in glossary but parking because there is no better home for it.

The short-coming of `Monad` is you can put two together but you don't get a new `Monad` instance out of it.

A **monad transformer** is a type constructor that takes a `Monad` as an argument and returns a `Monad` as a result.

**Transformers** help you make a monad out of multiple types that each have a Monad instance, by wrapping these existing monads, each of which provides a bit of the functionality you want.

**Transformers** are a means of avoiding making a one-off Monad for every possible combination of types.

