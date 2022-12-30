## Interpret again

> What is `fmap` doing in `ex2`

```
ex2 = fmap (>3) (+1)
```

> So consider the type

```
Int -> Bool
```

> One can think of `(Int ->)` as a thing which takes a type, here `Bool`, and “returns” the type `Int -> Bool`. In the same way, `Maybe` takes any type, e.g. `Bool`, and returns a new type, `Maybe Bool`.

- `(Int -> )` takes a type, e.g.  `Bool` and returns the type `Int -> Bool`.



**Rather interesting**






```haskell
fmap :: Functor f                      => (a -> b) -> f a -> f b
ghci> :t fmap (>3)
fmap (>3) :: (Functor f, Ord a, Num a) =>             f a -> f Bool
ghci> :t fmap (>3) (+1)
fmap (>3) (+1) :: (Ord a, Num a)       =>               a -> Bool
ghci> :t fmap (>3) (+1) 1
fmap (>3) (+1) 1 ::                                          Bool
```


I think I got your message up to the type signature you asked me to write a functor instance (call that Part 1). I was going to give you how I interpreted Part 1 but it led be to look at this and I'm wondering if I'm on to something here.

```haskell
ghci> aa = 3 :: Int

fmap :: Functor f =>              (a -> b) -> f a   -> f b      -- [A]

ghci> :t fmap (>aa)
fmap (>aa) :: Functor f =>                    f Int -> f Bool   -- [B]

ghci> :t fmap (>aa) (+1)
fmap (>aa) (+1) ::                            Int   -> Bool     -- [C]

ghci> :t fmap (>aa) (+1) 1
fmap (>aa) (+1) 1 ::                                   Bool     -- [D]
```

The interesting part is how the `f` goes away.

```haskell
fmap (>3)        :: f Int -> f Bool  -- [B]
fmap (<3) (+1)   ::   Int ->   Bool  -- it removed the Functor [C]
```

Nope - here is Rubin's reply

```haskell
fmap (>3)        :: f        Int -> f        Bool
fmap (<3) (+1)   ::                 (Int ->) Bool
(+1)             :: (Int ->) Int
```


```haskell
fmap (>3)        :: f        Int ->    f           Bool
fmap (<3) (+1)   ::                    (Int ->)    Bool
(+1)             :: (Int ->) Int
```


**The last and not the first two in which cas `(Int -> )` is a functor which is what he has been trying to say to me.**

**Also, if adding a param takes types from the left, how does that make you think about the above alignment?**

One way to look at that would be that the next param needed in [B] was a `Functor int` but when given `(+1)` it received a `Functor` but is still waiting for the `Int` part of `Int -> Bool`.

```haskell
:t (+1)
(+1) :: Num a => a -> a
```

So `(+1)` is kind `* -> *` so meets on prerequisite of a functor.

Am I getting there?





---

Here is the problem i ran into with AdventOfCode Day 4.

I kept getting structures inside of structures until it was so deeply nested I couldn't figure it out. Traversable addresses that.

```haskell
fmap :: Functor f
	=> (a -> b)
	-> f a
	-> f b
	
traverse :: Applicative f
	=> (a -> f b)
	-> t a
	-> f (t b)
	
myData :: [String]
myFunc :: String -> IO Record

wrong :: [IO Record] -- results in nesting
wrong = fmap myFunc myData

right :: IO [Record] -- not nested
right = traverse myFunc myData
```


# Sequence A

```haskell
sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)
sequenceA = traverse id
```

You can see from the type signature above that the effect of sequenceA is to flip two contexts or structures. It doesn’t by itself allow you to apply any function to the a value inside the structure. It only flips the layers of structure around. 

```haskell
sequenceA [Just 1, Just 2]
Just [1,2]
```

If I reiterate what you said in my own words I get this ??
- `(Int -> )` means "`Int` to something but we don't know what yet"
- `(Int -> Bool)` is a **type that takes an 'Int' and returns a 'Bool'**. 
- `(Int -> Bool)` is also a type that "represents a function"
- `Type -> Type` is equivelant to `* -> *`, you just sub-ing `Type` to make it clearer

Also
- `Int` has kind `*` and can't be a functor - got it. 
- A kind of `* -> *` is a prereq for a functor - got that too


Wrong
- "...thing which takes a type, here `Bool`, and “returns” the type `Int -> Bool`. NO CARL - this isn't **partial application**

Here is a stab in the dark
```haskell
instance Functor SomeType where
  fmap :: (a -> b) -> (Int -> a) -> (Int -> b)
  fmap x f1 f2 = f2 (f1 x)
```



I think `fmap :: (a -> b) -> (Int -> a) -> (Int -> b)` says


I see `fromInteger :: Num a => Integer -> a` is very similar


There are many functions from `(a -> b)`
So if I had an `(a -> a)` 

give me something and I'll give you a type

---

I looked in Hoogle for examples of `(Int -> a)` and `(a -> b)`. There is [`toId`](https://hackage.haskell.org/package/dejafu/docs/Test-DejaFu-SCT-Internal.html#v:toId) and various coerce functions but not much else.

`fromInteger :: Interger -> a` is very close.

`fromInteger :: Num a => Integer -> a` is similar but has a `Num` constrint.

A function `(a -> b)`
```haskell
aToB :: 
```



