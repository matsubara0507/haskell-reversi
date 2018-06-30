module Reversi.Coord where

-- | 座標やベクトルを表現する型
--   (x, y) であることに注意
type Coord = (Int, Int)

-- | x 座標と y 座標をそれぞれ加算する
(|+|) :: Num a => (a, a) -> (a, a) -> (a, a)
(x1, y1) |+| (x2, y2) = undefined

-- | x 座標と y 座標をそれぞれ減算する
(|-|) :: Num a => (a, a) -> (a, a) -> (a, a)
(x1, y1) |-| (x2, y2) = undefined
