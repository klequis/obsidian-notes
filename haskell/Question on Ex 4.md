**Ex 2**

Everything seemed to make sens here. 

```haskell
fmap sum [Just 1]

fmap sum :: (Functor f, Foldable t, Num b) => f (t b) -> f b
```

```haskell
(Functor f, Foldable t, Num b) =>  f   (  t    b  ) -> f b
        [1]         [2]    [3]    [1]    [2]  [3]
```

But then came **Ex 4**

```haskell
fmap sum [Just 1]

fmap sum          :: (Functor f, Foldable t, Num b)     => f (t b) -> f b
fmap sum []       ::                         Num b      =>            [b]
fmap sum [Just]   :: (Foldable ((->) a), Num (Maybe a)) => [Maybe a]
fmap sum [Just 1] ::                         Num b      =>            [b]
```

