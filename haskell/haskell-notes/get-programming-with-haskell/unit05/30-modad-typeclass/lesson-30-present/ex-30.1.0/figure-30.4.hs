-- Figure 30.4

-- 1. a value in a context
val :: Maybe Integer
val = Just 3

-- 2. a function that takes a value not in context
--    and returns a value in context
fn :: Num a => a -> Maybe a
fn x = Just (x * 3)

-- 3. >>= allows you to conbine them
a :: Maybe Integer
a = val >>= fn
-- > a
-- Just 9