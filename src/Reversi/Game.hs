module Reversi.Game where

import           Reversi.Board (Board, Move)
import qualified Reversi.Board as Board
import           Reversi.Piece (Piece (..))
import qualified Reversi.Piece as Piece
import           Reversi.Utils (untilM)

class Play p where
  play :: p -> Piece -> Board -> IO (Maybe Move)

data Game p1 p2 = Game
  { board :: Board
  , turn  :: Piece
  , black :: p1
  , white :: p2
  }

start :: (Play p1, Play p2) => p1 -> p2 -> IO ()
start p1 p2 = do
  game <- untilM isFinish step $ Game Board.new Black p1 p2
  displayBoard game
  putStrLn $
    case compare <$> Board.black <*> Board.white $ board game of
      LT -> "Winner is White"
      EQ -> "Draw"
      GT -> "Winner is Black"

isFinish :: Game p1 p2 -> Bool
isFinish game =
  ((+) <$> Board.black <*> Board.white $ board game) == Board.size * Board.size

step :: (Play p1, Play p2) => Game p1 p2 -> IO (Game p1 p2)
step game = do
  displayBoard game
  let ps  = turn game
      brd = board game
  mov <- case ps of
    Black -> play (black game) Black brd
    White -> play (white game) White brd
  let brd' = maybe (board game) (\m -> Board.doMove ps m brd) mov
  pure $ game { board = brd', turn = Piece.opponent ps }

displayBoard :: Game a b -> IO ()
displayBoard = putStrLn . Board.prityprint . board
