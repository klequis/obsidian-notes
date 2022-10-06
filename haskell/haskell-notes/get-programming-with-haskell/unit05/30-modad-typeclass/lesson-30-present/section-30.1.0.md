# Intro

1. What are the limitations of `Functor` and `Applicative`?

2. Example of `Monad`'s `(>>=)` operator to chain together functions in context.

3. Example of writing `IO` code without do-notation.


# 30.1 The limitations of Applicative and functor

- `Functor` allows you to change individual values in a context.
      - Fig 30.1
            - A value in context 
            - Is modified by a function not in context 
            - Resulting in a value in the same context
- `Applicative` allows you to use partial application in context.
      - Fig 30.2
            - A value in context
            - Is modified by a function in the same context
            - Resulting in a value in the same context
- `Monad` allows you to perform any arbitrary computation in a context of your choosing.
      - Fig 30.3
            - A vlue 



```haskell
fmap :: fn (f a) = f (fn a)
```

`fmap`
- A value in context
- A function not in context
- A mondified value in 