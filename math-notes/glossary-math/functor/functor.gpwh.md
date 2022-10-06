# Functor

*From Get Programming with Haskell, Lesson 27 p. 331*

- The **Functor type class** provides a generic interface for applying functions to values in a container or context.
- All functors are of kind `* -> *`. That is, parameterized types that take just one type parameter


A type `f` is a `Functor` if it 
- provides a function `fmap` 
- which, given any types `a` and `b` 
- lets you apply any function from `(a -> b)` 
- to turn an `f a` into an `f b`, 
- preserving the structure of `f`. 
- Furthermore `f` needs to adhere to the following:
  - Identity: `fmap id == id`
  - Composition `fmap (f. g == fmap f . fmap g`.

The *default* type signature for the `fmap` function is:
```haskell
fmap :: (a -> b) -> f a -> f b
```


### Example

Create an instance of `Functor` for type `Box a`
```haskell
data Box a = Box a deriving Show
```

```haskell
instance Functor Box where
  fmap f (Box val) = Box (f val)
```

To create an instance of `Functor` on a type
- create a `fmap` method
- that takes a function `f` and a type of `Box a` and apply `f` to the balue in `Box`
  - So the value in Box in the above is `val`

> I don't understand whey in this case since `Box a` takes a type `a` that the instance declaration isn't `instance Functor (Box a) where`

**Usage**

Converts `Box a` to `Box [a]`
```haskell
morePresents :: Box a -> Box [a]
morePresents (Box a) = Box [a]

box1 = Box 1
box2 = Box 2

fmap morePresents [box1,box2]
[Box [1],Box [2]]
```

---

So the book is making this point:
```haskell
successfulRequest :: Maybe Int
successfulRequest = Just 6

incMaybe :: Maybe Int -> Maybe Int
incMaybe (Just n) = Just (n + 1)
incMaybe Nothing = Nothing

```

To make use of `Maybe` in the `incMaybe` function (it just does `(+1)`) above you need to write a function that takes a `Maybe Int`.

But with `Functor` and `fmap` you don't need to:
```haskell
 > fmap (+1) successfulRequest
 Just 7
```

Here `successfulRequest` (a `Maybe Int` ) and `(+1)` are passed to the `fmap` function that already exists. You don't need to write a function. `fmap` is taking a `Maybe Int` and returning a `Maybe Int`.

And you can do this same thing with the **binary operator `<$>`**.
```haskell
> (+1) <$> successfulRequest
Just 7
```

Note that `fmap` doesn't need to return the same type it received. The below takes a `Maybe Int` and returns a `Maybe String`.
```haskell
> fmap (\x -> show x) (Just 5)
Just "5"
```

---

**Converting a `Maybe RobotPart` to `Maybe Html`.**

