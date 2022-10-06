import Data.Either
-- import Data.Int

-- e = size empty

data AType = A1 | A2 | A3
data BType = B1 | B2 | B3

aa :: Either AType BType -- has size == 6
aa = Left A1

bb :: Int-> Int -> Int
bb a b = a * b



x = maxBound :: Int