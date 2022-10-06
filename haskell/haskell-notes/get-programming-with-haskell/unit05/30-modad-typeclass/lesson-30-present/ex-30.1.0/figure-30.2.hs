-- Figure 30.2

-- 1. A value in context
val :: Maybe Integer
val = Just 3

-- 2. A function in the same context
fn :: Maybe (Integer -> Integer)
fn = Just (*3)

-- 3. A modified value in the same context
a :: Maybe Integer
a = fn <*> val