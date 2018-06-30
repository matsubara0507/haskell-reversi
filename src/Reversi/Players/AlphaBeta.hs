module Reversi.Players.AlphaBeta where

import           Reversi.Board
import           Reversi.Game  (Play (..))
import           Reversi.Piece

newtype AlphaBetaPlayer = AlphaBetaPlayer
  { depth :: Int
  }

instance Play AlphaBetaPlayer where
  play player piece board = do
    let mov = snd $ alphabeta piece board (-127, 127) (depth player)
    putStrLn $ maybe "Pass" showPos mov
    pure mov

evaluate :: Piece -> Board -> Int
evaluate Black = (-) <$> black <*> white
evaluate White = (-) <$> white <*> black

alphabeta :: Piece -> Board -> (Int, Int) -> Int -> (Int, Maybe Move)
alphabeta piece board (al, be) dep
  | dep == 0 || null movs = (evaluate piece board, Nothing)
  | otherwise = (,) <$> toScore <*> toMove $ foldr (choiceBest piece board dep be) ini movs
  where
    movs = moves piece board
    ini  = St (-127) Nothing al False

data LoopState = St
  { toScore :: Int
  , toMove  :: Maybe Move
  , toAlpha :: Int
  , isBreak :: Bool
  }

choiceBest :: Piece -> Board -> Int -> Int -> Move -> LoopState -> LoopState
choiceBest piece board dep be mov st
  | isBreak st = st
  | otherwise  = St score mov' alpha (alpha >= be)
  where
    board'        = doMove piece mov board
    (score', _)   = alphabeta (opponent piece) board' (-be, -(toAlpha st)) (dep - 1)
    (score, mov') = if (-score') > toScore st then (-score', Just mov) else (toScore st, toMove st)
    alpha         = max (toAlpha st) (-score')
