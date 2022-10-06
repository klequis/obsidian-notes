# #return

```haskell
return :: a -> m a
```

`return` takes a value of a type and puts it in the context of a `Monad`.

> `return` is a function that takes any type of value and makes an action that results in that value.
> -- [Introduction to Haskell IO/Actions](https://wiki.haskell.org/Introduction_to_Haskell_IO/Actions#:~:text=Actions%20are%20values%20in%20the,every%20action%20has%20a%20type.)


>[!WARNING]
>- `return` does not affect the control flow of the program
>- It does not break the execution of the do-block
>- It doesn't directly contribute to the result of the do-block
>- `return` is simply a function that makes an action whose result is a particular value.

