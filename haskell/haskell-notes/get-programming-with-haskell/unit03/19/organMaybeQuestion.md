In 
```haskell
processAndReport :: Maybe Organ -> String
processAndReport (Just organ) = report (process organ)
processAndReport Nothing = "error, id not found"
```

I'm not understanding the `processAndReport (Just organ) = report (process organ)` line.

I think I see what is happening but I don't understand how.

If a `Just organ` is passed to `procesAndReport` then `organ` is passed to `process` which expects an `Organ` - all good there.

```haskell
process :: Organ -> (Location, Container)
process organ = placeInLocation (organToContainer organ)
```

It appears that somehow the `organ` is being taken out of `Just organ` so it can be passed to `process`. However, I'm left asking myself 'what mechanism' or 'Haskell thingy' is makeing that happen.

I don't think it is needed, but just in case, here is the entire code sample:

```haskell
import qualified Data.Map as Map
import Data.List ( intercalate )

data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq)

organs :: [Organ]
organs = [Heart,Heart,Brain,Spleen,Spleen,Kidney]

ids :: [Int]
ids = [2,7,13,14,21,24]

organPairs :: [(Int,Organ)]
organPairs = zip ids organs

-- use `Map.lookup 7 organCatalog` to get an organ
organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs
  
possibleDrawers :: [Int]
possibleDrawers = [1 .. 50]

getDrawerContents :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawerContents ids catalog = map getContents ids
	where getContents = \id -> Map.lookup id catalog

availableOrgans :: [Maybe Organ]
availableOrgans = getDrawerContents possibleDrawers organCatalog

countOrgan :: Organ -> [Maybe Organ] -> Int
countOrgan organ available = length (filter
										(\x -> x == Just organ)
									available)

  

isSomething :: Maybe Organ -> Bool
isSomething Nothing = False
isSomething (Just _) = True

justTheOrgans :: [Maybe Organ]
justTheOrgans = filter isSomething availableOrgans

showOrgan :: Maybe Organ -> String
showOrgan (Just organ) = show organ
showOrgan Nothing = ""

organList :: [String]
organList = map showOrgan justTheOrgans

cleanList :: String
cleanList = intercalate ", " organList

data Container = Vat Organ | Cooler Organ | Bag Organ

instance Show Container where
	show (Vat organ) = show organ ++ " in a vat"
	show (Cooler organ) = show organ ++ " in a cooler"
	show (Bag organ) = show organ ++ " in a bag"

data Location = Lab | Kitchen | Bathroom deriving Show

organToContainer :: Organ -> Container
organToContainer Brain = Vat Brain
organToContainer Heart = Cooler Heart
organToContainer organ = Bag organ

placeInLocation :: Container -> (Location,Container)
placeInLocation (Vat a) = (Lab, Vat a)
placeInLocation (Cooler a) = (Lab, Cooler a)
placeInLocation (Bag a) = (Kitchen, Bag a)

process :: Organ -> (Location, Container)
process organ = placeInLocation (organToContainer organ)

report ::(Location,Container) -> String
report (location,container) = show container ++
								  " in the " ++
								  show location

processAndReport :: Maybe Organ -> String
processAndReport (Just organ) = report (process organ)
processAndReport Nothing = "error, id not found"
```