#overloadedstrings 

I think the short story here is:

The `OverloadedStrings` extension lets you ... wait for it ... overload strings.

Sadly it is often introduced as a way to automatically use `String` as `Text`,  i.e., automatically as in you don't have to use `Data.Text.pack`.

As it turns out, this appealingly automatic conversion from `String` to `Text` is just one example of "overloading a string". And, to overload a string you must use the `OverloadedStrings` extension.

```haskell
$ ghci
> a = "Hello" :: Text
<interactive>:2:16: error:
    Not in scope: type constructor or class ‘Text’
```

That was expected. You need to import `Data.Text`.

```haskell
> import Data.Text
> a = "Hello" :: Text
• Couldn't match type ‘[Char]’ with ‘Text’
      Expected: Text
        Actual: String
```

Importing `Data.Text` solved the 'not in scope' problem but we are still unable to convert `String` to `Text` without using `Data.Text.pack`.

```haskell
> :set -XOverloadedStrings
> a = "Hello" :: Text
> :t a
a :: Text
```

That works. But why?

I'll exit GHCi and go back in again so things are reset

```haskell
> :q
$ ghci
>
```

```haskell
> :t "Hello"
"Hello" :: String
```

`"Hello"` is a `String`. If I import `Data.Text`

```haskell
> import Data.Text
ghci> 
ghci> :t "Hello"
"Hello" :: String
```

That made no difference. `"Hello"` is still a `String`. Next, set `XOverloadedStrings`

```haskell
> :set -XOverloadedStrings
> :t "Hello"
"Hello" :: Data.String.IsString p => p
```

Now, `"Hello"` is something different. It is a `Data.String.IsString p => p`. Let's look at the `IsString` class.

---

# (this isn't finished or of value yet) Some questions bout `IsString`

First, I'm curious about the instance `IsString Text`
```haskell
instance IsString Text where
    fromString = pack
```

I thought using `OverloadedStrings` with `Data.Text` was both a convenience and a performance gain. Is it that all the calls to `pack` happen at compile time?

`fromString` seems pretty straight forward. It takes a `String` and turns it into something.
