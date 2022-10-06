## Progress
- So far I understand that a lot of code was written to combine two `PTable`s (or is it sets of probabilities?).
- I likely understand `CreatePTable` but should go through it again.
- Then I need to go through the code that combines the two `PTable`s.


**Types and data**
```haskell
type Events = [String]
type Probs = [Double]

data PTable = PTable Events Probs
```
---
**showPair**
```haskell
showPair :: String -> Double -> String
showPair event prob = mconcat [event,"|", show prob,"\n"]
```
- receives a string and a double
- uses `show` to convert the double to a string
- uses `mconcat` to combine strings
- E.g.,
	- > showPair "heads-red" 0.1
	- "heads-red|0.1"
---
**instance Show PTable**
```haskell
instance Show PTable where
	show (PTable events probs) = mconcat pairs
		where pairs = zipWith showPair events probs

```
- Is makeing PTable an instance of Show.
- The instance has a `show` method.
- `events` & `probs` are lists
- uses `showPair` to zip the lists
- **Speculation:** When Haskell needs to show a PTable it will find the instance for `Show PTable`, populate the params `events` & `probs`, and then execute the `show` method. *(seems obviously true)*
---
**createPTable**
```haskell
createPTable :: Events -> Probs -> PTable
createPTable events probs = PTable events normalizedProbs
	where totalProbs = sum probs
		  normalizedProbs = map (\x -> x/totalProbs) probs
```
- `events` & `probs` are lists.
- The returned `PTable` has `events` for the `Events` data constructor and `normalizedProbs` for the `Probs` data constructor.
> [!question] I'm wondering if the data constructors hold/contain the data?
---

**cartCombine**
```haskell
cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func newL1 cycledL2
	where nToAdd = length l2
		  repeatedL1 = map (take nToAdd . repeat) l1
		  newL1 = mconcat repeatedL1
		  cycledL2 = cycle l2
```
- Combines two lists.
- Takes a function, a list of `a`, a list of `b` and returns a list of `c`

So say this is the data
```haskell
l1 = [0.125,0.125,0.125,0.125,0.125,0.125,0.125,0.125]

l2 = ["heads-heads-heads","heads-heads-tails","heads-tails-heads","heads-tails-tails","tails-heads-heads","tails-heads-tails","tails-tails-heads","tails-tails-tails"]
```

- `nToAdd` = 8
- 

---

**combineEvents**
```haskell
combineEvents :: Events -> Events -> Events
combineEvents e1 e2 = cartCombine combiner e1 e2
	where combiner = (\x y -> mconcat [x,"-",y])
```

**combineProbs**
```haskell
combineProbs :: Probs -> Probs -> Probs
combineProbs p1 p2 = cartCombine (*) p1 p2
```

**instance Semigroup PTable**
```haskell
instance Semigroup PTable where
(<>) ptable1 (PTable [] []) = ptable1
(<>) (PTable [] []) ptable2 = ptable2
(<>) (PTable e1 p1) (PTable e2 p2) = createPTable newEvents newProbs
where newEvents = combineEvents e1 e2
newProbs = combineProbs p1 p2
```

**instance Monoid PTable**
```haskell
instance Monoid PTable where
mempty = PTable [] [] -- identity
mappend = (<>) -- append
```

**coin**
```haskell
coin :: PTable
coin = createPTable ["heads","tails"] [0.5,0.5]
```

**spinner**
```haskell
spinner :: PTable
spinner = createPTable ["red","blue","green"] [0.1,0.2,0.7]
```
