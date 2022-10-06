
```haskell
import Control.Monad

(.) :: (b -> c) -> (a -> b) -> a -> c
(>>=) :: Monad m => m a -> (a -> m b) -> m b
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
flip (.) ::         (a ->   b) -> (b ->   c) -> a ->   c
```