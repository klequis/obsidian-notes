import Control.Applicative
import Control.Monad
import Data.Functor.Identity

digit :: (Ord a, Num a, Show a) => a -> [Char] -> Maybe a
digit i (c:_)
  | i > 9 || i < 0 = Nothing
  | otherwise      =
    if [c] == show i then Just i else Nothing

a :: Maybe Integer
a = digit 5 "123456"
b :: Maybe Integer
b = digit 5 "5123412"

binChar :: String -> Maybe Int
binChar s = digit 0 s <|> digit 1 s <|> digit 3 s

