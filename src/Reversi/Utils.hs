module Reversi.Utils where

-- | monadic `until` function
untilM :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m a
untilM p f = go
  where
    go x
      | p x       = pure x
      | otherwise = go =<< f x
