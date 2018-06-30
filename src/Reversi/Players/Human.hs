module Reversi.Players.Human
  ( Human (..)
  ) where

import           Control.Monad (forM_)
import           Data.Maybe    (isJust)
import           Reversi.Board (moves, showPos)
import           Reversi.Game  (Play (..))
import           Reversi.Utils (untilM)
import           Text.Read     (readMaybe)

data Human = Human

instance Play Human where
  play _player piece board
    | null movs = pure Nothing
    | otherwise = do
        forM_ (zip [0 :: Int ..] movs) $ \(idx, mov) ->
          putStrLn $ concat [show idx, ") ", showPos mov]
        fmap (movs !!) <$>
          untilM (isJust . fmap (< len)) (\_ -> readMaybe <$> getLine) Nothing
    where
      movs = moves piece board
      len  = length movs
