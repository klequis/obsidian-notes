_ :: forall (f :: * -> *) a b. Functor f => (a -> b) -> f a -> f b


$dFunctor :: Functor ((->) [Integer])                                  | $dFunctor :: Functor Maybe
_ :: (Maybe [Integer] -> Maybe Integer)                                | 
     -> ([Integer] -> Maybe [Integer]) -> [Integer] -> Maybe Integer   | _ :: ([Integer] -> Integer) -> Maybe [Integer] -> Maybe Integer

eg = (fmap . fmap) sum Just [1,2,3]

- The lefthand fmap in [B] is instantiated to the ((->) [Integer]) functor.
- The righthand fmap in [B] is instantiated to the Maybe functor.

- Note that HLS gives both the general type of fmap, and the specific type, so it’s extremely useful for answering questions like the ones here.


---

-- ((->) [Integer]) functor (LHS)
_ ::                                        (Maybe [Integer] -> Maybe Integer) -> ([Integer] -> Maybe [Integer]) -> [Integer] -> Maybe Integer
_ :: forall (f :: * -> *) a b. Functor f => (a               -> b            ) -> f a ->                            f b


-- Maybe functor (RHS)
_ ::                                        ([Integer] -> Integer) -> Maybe [Integer] -> Maybe Integer
_ :: forall (f :: * -> *) a b. Functor f => (a ->         b      ) -> f     a         -> f     b


