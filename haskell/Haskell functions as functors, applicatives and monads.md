source; [Haskell functions as functors, applicatives and monads](https://eli.thegreenplace.net/2018/haskell-functions-as-functors-applicatives-and-monads/ "Permalink to Haskell functions as functors, applicatives and monads")

# Functor

**statement: All 3 are the same**
```haskell
fmap :: (b -> c) -> FuncWithArgA b -> FuncWithArgA c
fmap :: (b -> c) -> ((->) a) b     -> ((->) a) c
fmap :: (b -> c) -> (a -> b)       -> ( a -> c)
```


Why does w/ parens return `[String]` while w/o parens returns `String`?

```haskell
fmap                       ::  Functor f          => (a -> b) -> f a -> f b
fmap show                  :: (Functor f, Show a) =>             f a -> f String
fmap show  replicate4      ::             Show a  =>                    a -> String   -- without  [A]
fmap show (replicate4)     ::             Show a  =>                    a -> String   -- with     [B]
-- diff starts below                                                 --
fmap (show  replicate4) 2  ::                                                String   -- without  [C]
fmap  show (replicate4  2) ::                                               [String]  -- with     [D]
```

**Observations**
[A] - `a -> String` is not `Int -> String` as I at first assumed because the type of `replicate` is
```haskell
replicate :: Int -> a -> [a]
```
And `a` is `Any`

```haskell
fmap           ::  Functor f                => (a -> b) -> f a -> f b
fmap (>3)      :: (Functor f, Ord a, Num a) =>             f a -> f Bool
fmap (>3) (+1) :: (           Ord a, Num a) =>                    a -> Bool
```

Having trouble understanding whey the below are different

```haskell
replicate4 :: a -> [a]
replicate4 = replicate 4

fmap show  replicate4 2  :: String
"[2,2,2,2]"

fmap show (replicate4 2) :: [String]
["2","2","2","2"]
```

OK you dummy. Here it is

```haskell
fmap show replicate4 4 == (show . replicate4) -- that is all of it
"[2,2,2,2]"

fmap show (replicate4 2) == fmap show [2,2,2,2]
["2","2","2","2"]
```

There isn't anything more to it!



### Write about it

If you remember that `fmap fn1 fn2` is the same as `(fn1 . fn2)` then this appears easy for me. Here is another example:

What is the difference between these two equations?

```haskell
ghci> fmap show (replicate4 2)  -- [A]
["2","2","2","2"]

ghci> (fmap show replicate4) 2  -- [B]
"[2,2,2,2]"
```


I find [A] easier to understnad
- `replicate 2` creates `[2,2,2,2]`
- then `fmap` maps `show` over `[2,2,2,2]`
- to produce ["2","2","2","2"]

[B] I find less intuitive. The trick is to remember that mapping a function over a function is composition `(fn1 . fn2)` same as `fn1(fn2)`. With that in mind:
- \[B] is `(show . replicate4) 2`
- so when `replicate4` is applied to `2` you get `[2,2,2,2]`
- then `show [2,2,2,2]` is `"[2,2,2,2]"`

### Paramerritized by the return type

#### The return type is "a parameter"


- `(->)` is parameeterized by two types `(->) a b`
	- `a` is the function argument
	- `b` is the return type
	- `a` and `b` can have arbitrary types.
- `(->) a` is a type parameterized by a single type
	- its return type is fixed at `a`
	- It is a "partially applied type"


**"fix the argument type"**, **"leaving tye return type arbitrary"**
- written as `(->) a`
	- "a type parameterized by a single type" == "



 ```haskell
fmap (>3) (+1) == (>3) . (+1)

fmap g ff = \x -> g (ff x)
```
- `g` is `(>3)`
- `ff` is `(+1)`
- It is waiting for a `Num`, i.e. the `x`
```haskell
	ghci> :t fmap (>3) (+1)
	fmap (>3) (+1) :: (Ord a, Num a) => a -> Bool       -- [A]

	ghci> :t :t (\x -> (>3)((+1)x))
	(\x -> (>3)((+1)x)) :: (Ord a, Num a) => a -> Bool  -- [B]

	ghci> :t ((>3) . (+1))
	((>3) . (+1)) :: (Ord b, Num b) => b -> Bool        -- [C]


	-- ** THEY ARE ALL THE SAME TYPE **
	fmap   (>3)   (+1)    :: (Ord a, Num a) => a -> Bool  -- [A]
	(\x -> (>3)  ((+1)x)) :: (Ord a, Num a) => a -> Bool  -- [B]
	      ((>3) . (+1))   :: (Ord b, Num b) => b -> Bool  -- [C]
```
- its return type is already fixed as a `Bool`

### Functor Laws

```haskell
fmap id = id                     -- idenity
fmap (g . h) = fmap g . fmap h   -- composition
```

#### For functions, what is `id` for `fmap`?

#### Identity

Write out the curried form of `fmap` for functions
```haskell
fmap g ff = \x -> g (ff x)
-- can be written as
fmap g = \ff -> \x -> g (ff x)
```

And then `fmap id` is
```haskell
fmap id = \ff -> \x -> id (ff x)
-- or
fmap id = \ff -> \x -> ff x
-- or
fmap id ff x = ff x
```
So `fmap id` takes a function and returns that function

Example:
```haskell
> zz = fmap id (+3)
> :t zz
zz :: Num b => b -> b
> zz 2
5
-- zz is clearly (+3)
```

#### Composition

```haskell
fmap (g . h) = fmap g . fmap h
```


```haskell
(fmap g . fmap h) ff = fmap g (fmap h ff)
					 = fmap g (\x -> h (ff x))      -- by definition of "fmap h ff"
					 = \y -> g ((\x -> h (ff x)) y) -- by definition of "fmap g ff" for ff now being (fmap h ff)
					 = \y -> g (h (ff y))
```

Look at it again
```haskell
-- `g`, `h` & `ff` are all functions

(fmap g . fmap h) ff = fmap g (fmap h ff)           -- [A] the composition of `(fmap g . fmap h) ff`
					 = fmap g (\x -> h (ff x))      -- [B] by definition of "fmap h ff"
					 = \y -> g ((\x -> h (ff x)) y) -- [C] by definition of "fmap g ff" for ff now being (fmap h ff)
					 = \y -> g (h (ff y))
```


```haskell
(fmap  g   . fmap  h)    ff  = fmap  g   (\x ->  h    (ff  x)) -- [B]
(fmap (>3) . fmap (+1)) (*2) = fmap (>3) (\x -> (+1) ((*2) x)

(fmap  g   . fmap  h)    ff    = \y -> (>3) ((\x -> h    (ff   x)) y) -- [C]
(fmap (>3) . fmap (+1)  (*2*)) = \y -> (>3) ((\x -> (+1) ((*2) x)  y))
```

# Applicative
*Function as an instance of Applicative*


```haskell
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```

## `pure` for `instance Applictive (->) a`

Considering `pure`

Remember you can't have `(->)` as a functor because it has the wrong kind, `* -> * -> *` instead of `* -> *`. Therefore you use a partially applied function with kind `* -> *`. In the article she is using `FuncWithArgA` which is `(-> a)`.

Take `pure` and substitue `(-> a)` for `f`
```haskell
pure :: a -> f a
pure :: b -> (a -> b) -- remember FunctWithArgA is ((->) a) which is (a ->)
-- given an `b` you ignore the `a` and get a `b`
-- HEY - that's `const`
-- you can write it like this
pure = \x -> (\y -> x)
```

So `pure` for a partially applied function is `const`

## `<*>` for `instance Applicative (->) a`

```haskell
(<*>) :: FuncWithArgA (b -> c)  -> FuncWithArgA b  -> FuncWithArgA c
(<*>) :: (a        -> (b -> c)) -> (a        -> b) -> (a        -> c)
(<*>) :: (r ->        (a -> b)) -> (r        -> a) ->  r        -> b
g <*> h = \x -> g x (h x)
```

### This seems related: use-cases-for-functor-applicative-monad-instances-for-functions

source: https://stackoverflow.com/questions/46631242/use-cases-for-functor-applicative-monad-instances-for-functions

I ran into this along the way and should get a better understainding of it.
```haskell
ea = (+) <$> (*2) <*> subtract 1

eb = fmap (+) (*2) <*> subtract 1

```

```haskell
fmap (+) (*2)          :: Num a => a -> a -> a
fmap (+) (*2) <*> (+1) :: Num a =>      a -> a
```

```haskell
-- if
g <*> h = \x -> g x (h x)
-- then
(+1) <*> (*2) = (x+1)

(+1) <*> (*2) = \x -> (+1)x ((*2)x)
```

ouch. `g` takes 2 args and `h` takes 1
```haskell
(a -> (b -> c)) -> (a -> b) -> (a -> c)
(+)             -> (*2)

g   <*> h    = \x -> g x (h x)
(+)     (*2) = \x -> (+) x ((*2)x)
```

---

not getting it yet. try this

```haskell
(<*>) :: (r -> (a -> b)) -> (r -> a) -> r -> b
--            [A]              [B]     [C]  [D]
```

[A] 
- is a function that takes 2 arguments `(r -> a -> b)`
- when you give it 1 argument it is not a function that takes 1 argument `(a -> b)`

[B]
- `r` is something of the same type as the `r` in [A]
- given an `r` it will return an `a`

[C] - [D]
- Now you give it an `r` which goes to the `r` in both [A] and [B] (seems wrong)
- and you get a `b`

Note that [B] is `r -> a`
and [A] is          `r -> a -> b`
so [B] is the `a -> b` in [A] (I think, maybe)

At any rate note that

- `(+)`   is `r -> b -> c` which of course can be `a -> a -> a`
- `(*2)` is `r -> b`           which of course can be `a -> a`
- what you get back is a function `(r -> c)`
```haskell
(r -> (b -> c)) -> (r -> b) -> (r -> c)
(+)             -> (*2)
g        <*>        h       = \x -> (+) x ((*2)x)
```

```haskell
-- If I give `(+)` to
(r -> (b -> c))
-- do I get (pseudo), which is waiting for a `(b -> c)`
((+) -> (b -> c))
-- and is that `(b -> c)` `(*2)`
((+)) -> (*2))
-- not really but kind of?
```

OK, I'm done with this. Back to PHFFP.
