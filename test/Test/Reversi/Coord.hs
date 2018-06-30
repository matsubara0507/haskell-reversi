module Test.Reversi.Coord where

import           Reversi.Coord

scprop_coordAdd :: (Int, Int) -> (Int, Int) -> Bool
scprop_coordAdd (x1, y1) (x2, y2) = (x1, y1) |+| (x2, y2) == (x1 + x2, y1 + y2)

scprop_coordSub :: (Int, Int) -> (Int, Int) -> Bool
scprop_coordSub (x1, y1) (x2, y2) = (x1, y1) |-| (x2, y2) == (x1 - x2, y1 - y2)
