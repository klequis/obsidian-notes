-- Figure 30.1

-- 1. a value in context
val :: Maybe Integer
val = Just 3

-- 2. a function not in context
fn :: Num a => a -> a
fn x = 3 * x

-- 3. a modified value in the same context
a :: Maybe Integer
a = fmap fn val

b :: Maybe Integer
b = fn <$> val


