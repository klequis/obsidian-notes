This question arises from a couple of books that keep mentioning "inheritance". Having done a decade of OO my mind has a very fixed idea of inheritance and I'm trying to clear-up what it means in Haskell:
1. Is there a thing called inheritance implemented in GHC?
2. Or is the word just used for lack of a better one?

I'm trying to understand the relationship between data types/declarations, classes and instances.

I'm looking at [data Double](https://hackage.haskell.org/package/base-4.16.0.0/docs/Prelude.html#t:Double) and [class Eq a](https://hackage.haskell.org/package/base-4.16.0.0/docs/Prelude.html#t:Eq) and both show the instance `Eq Double`. However, my understanding is there is only one instance and searching GHC code for "instance Eq Double" only shows up once in `libraries/ghc-prim/GHC/Classes.hs`. 

So I'm wondering if the `instance Eq Double` is part of `class Eq a`, `data Double` or really neither.

The language used by books and articles is confusing (maybe conflicting) and uses terms like "inherited" and "member of" so I haven't been able to get a clear answer.

Also, a side question of interest to me but not really necessary to know, I don't see in https://github.com/ghc/ghc.git the declaration for `class Eq a`. I did find it in docs under [Data.Eq](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Eq.html) and the uri indicates in is in the 'base' package but I don't see a base package on github.

