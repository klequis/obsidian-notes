# Legend

#### Ruben said it

> I claim it


---

## Comments/? to Ruben
- How do you lookup `(a -> )` on hackage. Hackage looks like ... and make sure `(a -> )` is the thing he actually mentioned.


> **`->` is a type constuctor**
> 
> it also and associates to the right.

> `->` signals the need for application at both term and type level


## `(Int ->)`

#### In Haskell code, must be written as `( -> ) Int`

#### Is of kind `* -> *`
```haskell
ghci> :k (-> ) Int
(-> ) Int :: * -> *
```

The kind of (->) is `* -> * -> *`
```haskell
ghci> :k (-> )
(-> ) :: * -> * -> *
```

So the types fill in from the left
```haskell
ghci> :k (->)
(->) :: * -> * -> *

ghci> :k (->) Int
(->) Int :: * -> *

ghci> :k (->) Int Bool
(->) Int Bool :: *

```

And this is the same as variables for a function
```haskell
fn1 :: Int -> Char -> Bool -> (Int,Char,Bool)
fn1 a b c = (a,b,c)

> aa = fn1 True
• Couldn't match expected type ‘Int’ with actual type ‘Bool’

> aa = fn1 1
> :t aa
aa :: Char -> Bool -> (Int, Char, Bool)
```


Tangent: Maybe a bit obvious but all these types are **concrete**
```haskell
fn1 :: Int -> Char -> Bool -> (Int,Char,Bool)
fn1 a b c = (a,b,c)

> aa = fn1 1
> :t aa
aa :: Char -> Bool -> (Int, Char, Bool)

> :k Char -> Bool -> (Int, Char, Bool)
Char -> Bool -> (Int, Char, Bool) :: *

> :k Int -> Char -> Bool -> (Int, Char, Bool)
Int -> Char -> Bool -> (Int, Char, Bool) :: *
```



```haskell
> (fmap . fmap) sum Just [1, 2, 3] -- [B] Just 6
Just 6


(_ . fmap) sum Just [1,2,3]
?[Just 1, Just 2, Just 3]


```

#### You give it a type and it will return a concrete type

```haskell
ghci> :k (-> ) Int Bool
(-> ) Int Bool :: *
```

> So can you saya that `(Int ->)` takes a type `a` and returns a type `(Int -> a)`

#### `(a ->)` is a functor

> `(Int ->)` is a specialized case of `(a ->)`



- `(a ->)` is a functor.
- One can think of (Int ->) 
	- as a thing which takes a type, here Bool, 
	-  and “returns” the type Int -> Bool.
	- **`(Int -> ` takes a type and returns a type `Int -> a`**


---

```haskell
(fmap (+1) negate) 10 == ((+1) . negate) 10

Integer -> Integer
fmap (+1) negate


negate :: Num a => a -> a
```

```haskell
(+1) :: Num a => a -> a

negate :: Num a => a -> a
```