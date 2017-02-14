module Utility (
    (<.>)
  , uncurry3
  ) where

uncurry3 :: (a -> b -> c -> d) -> ((a,b,c) -> d)
uncurry3 f (a,b,c) = f a b c

-- this is to (.) what (<$>) is to ($)
(<.>) :: Functor f => (b -> c) -> (a -> f b) -> (a -> f c)
(<.>) f g = fmap f . g
infixr 9 <.>

