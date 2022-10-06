So here is another example from a book

```
> createPTable ["red","blue","green"] [0.1,0.2,0.7]
red|0.1
blue|0.2
green|0.7
```

```haskell
type Events = [String]
type Probs = [Double]
data PTable = PTable Events Probs

instance Show PTable where
show (PTable events probs) = mconcat pairs
	where pairs = zipWith showPair events probs

createPTable :: Events -> Probs -> PTable
createPTable events probs = PTable events normalizedProbs
	where totalProbs = sum probs
		  normalizedProbs = map (\x -> x/totalProbs) probs
```

So `createPTable` is pretty simple. It takes events and probs and returns a `PTable` with `events` as passed in and `normalizedProbs`.

To show it, create an instance of `Show PTable`. which has a `show` method.

**Speculation:** When Haskell needs to show a PTable it will find the instance for `Show PTable`, populate the params `events` & `probs`, and then execute the `show` method.

I think that is it.

So are data constructors containers for data or not?
	I haven't been able to find an answer to that yet.


