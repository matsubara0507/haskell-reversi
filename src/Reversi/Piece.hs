module Reversi.Piece where

-- | 石の色を表す型
data Piece
  = Black
  | White
  deriving (Show, Eq)

-- | 逆の色を返す関数
opponent :: Piece -> Piece
opponent Black = White
opponent White = Black
