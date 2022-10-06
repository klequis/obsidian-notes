-- Figure 30.3

-- 1. A value in context
val :: Maybe Integer
val = Just 3

-- 2. A function that takes a value in context and returns
--    a modified value not in context
-- fn :: Maybe a -> a

-- fn :: Maybe (Maybe a) -> Maybe a
fn :: Maybe a -> a
fn (Just x) = x
-- ignore the Nothing case

-- 3. A value in context
a :: Applicative f => f a -> (f a -> b) -> f b
a x fn = pure (fn x)
-- > a val fn
-- Just 3
