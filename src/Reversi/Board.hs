module Reversi.Board
  ( -- * Board
    Board (..)
  , size
  , new
  , getFlip
  , getMove
  , moves
  , doFlip
  , doMove
    -- * Move
  , Move (..)
  , zeroFlips
  , isLegal
    -- * Others
  , hasCoord
  , (!!!)
  , directions
  , prityprint
  , showPos
  ) where

import           Data.Matrix   (Matrix, (!))
import qualified Data.Matrix   as Matrix
import           Data.Maybe    (fromMaybe, isNothing)
import           Reversi.Coord
import           Reversi.Piece

-- | 盤面を表す型
data Board = Board
  { matrix :: Matrix (Maybe Piece) -- ^ 盤面の石の状態を保持する
  , black  :: Int                  -- ^ 黒の石の個数
  , white  :: Int                  -- ^ 白の石の個数
  } deriving (Show, Eq)

-- | 盤面の1辺の長さの定数
size :: Int
size = 8

-- | 盤面の初期状態を返す
new :: Board
new = Board
  { matrix = mat
  , black  = 2
  , white  = 2
  }
  where
    (n, b, w) = (Nothing, Just Black, Just White)
    mat = Matrix.fromLists
      [ [n, n, n, n, n, n, n, n]
      , [n, n, n, n, n, n, n, n]
      , [n, n, n, n, n, n, n, n]
      , [n, n, n, w, b, n, n, n]
      , [n, n, n, b, w, n, n, n]
      , [n, n, n, n, n, n, n, n]
      , [n, n, n, n, n, n, n, n]
      , [n, n, n, n, n, n, n, n]
      ]

-- | 「手」を表す型
data Move = Move
  { pos   :: Coord -- ^ 手を打つ座標
    -- | 8方向それぞれの、'pos' に打った際にひっくり返すことができる石の数
    -- 'directions' と同じ順番で並んでいる
  , flips :: [Int]
  } deriving (Show, Eq)

-- | 1つもひっくり返すことのできない手の 'flips' の値
zeroFlips :: [Int]
zeroFlips = replicate size 0

-- | その手が合法、すなわち1以上の石をひっくり返せるとき 'True'
isLegal :: Move -> Bool
isLegal = (/= zeroFlips) . flips

-- | 第一引数に与えられた座標が盤面のサイズに収まっているとき 'True'
hasCoord :: Coord -> Bool
hasCoord (x, y) = 0 < x && x <= size && 0 < y && y <= size

{- | 第二引数に与えられた座標の状態を返す
     座標が盤面の範囲外であった場合は Nothing が返る。

     参照: https://hackage.haskell.org/package/matrix/docs/Data-Matrix.html
-}
(!!!) :: Matrix (Maybe Piece) -> Coord -> Maybe Piece
mat !!! (x, y) = undefined


-- | 隣接する8マスを指すそれぞれのベクトル
--
-- >    +---+---+---+
-- >    | 0 | 1 | 2 |
-- >    +---+---+---+
-- >    | 3 | X | 4 |
-- >    +---+---+---+
-- >    | 5 | 6 | 7 |
-- >    +---+---+---+
directions :: [Coord]
directions =
  [ (-1, -1) -- 左上
  , ( 0, -1) -- 真上
  , ( 1, -1) -- 右上
  , (-1,  0) -- 真左
  , ( 1,  0) -- 真右
  , (-1,  1) -- 左下
  , ( 0,  1) -- 真下
  , ( 1,  1) -- 右下
  ];


-- | 指定の色の石を指定の位置に置いたとき、指定の方向へひっくり返せる石の数を返す
getFlip
  :: Board
  -> Piece -- ^ 置く石の色
  -> Coord -- ^ 石を置く座標
  -> Coord -- ^ ひっくり返す石を探す方向
  -> Int   -- ^ ひっくり返せる石の数
getFlip board piece ps dir = undefined

-- | 指定の色の石を指定の位置に置いたときの「手」を返す
--   戻り値の `Move` には8方向分の `get_flip` の結果が含まれる
getMove :: Board -> Piece -> Coord -> Move
getMove board piece ps = undefined

-- | 合法な Move のリストを返す
--   盤面の左上から右下まで走査して、合法手を探し出す
moves :: Piece -> Board -> [Move]
moves piece board = undefined

-- | 石を指定の位置から指定の方向へ指定の数だけ指定の色にひっくり返す
--   ひっくり返した分だけ `black`/`white` の数を増減させる必要がある
doFlip
  :: Piece -- ^ ひっくり返したあとの色
  -> Coord -- ^ 石を置く位置
  -> Coord -- ^ ひっくり返す方向。`directions` の要素のいずれかが渡される
  -> Int   -- ^ ひっくり返す枚数
  -> Board -- ^ ひっくり返す前の盤面
  -> Board -- ^ ひっくり返した後の盤面
doFlip piece ps dir flipNum board = undefined

-- | 指定の色で指定の「手」を打つ
doMove :: Piece -> Move -> Board -> Board
doMove piece (Move ps ns) board = undefined

-- | 盤面をいい感じに出力するための関数
prityprint :: Board -> String
prityprint board = unlines $
  [ "  a b c d e f g h "
  , " +-+-+-+-+-+-+-+-+"
  ] ++ zipWith ppRow [1 :: Int ..] (concat <$> Matrix.toLists mat)
  where
    mat = fmap ppPiece (matrix board)
    ppPiece (Just Black) = "X|"
    ppPiece (Just White) = "O|"
    ppPiece Nothing      = " |"
    ppRow idx row = show idx ++ "|" ++ row ++ "\n +-+-+-+-+-+-+-+-+"

-- | 「手」をいい感じに出力するための関数
showPos :: Move -> String
showPos (Move (x, y) _) = (' ' : ['a'..]) !! x : show y
