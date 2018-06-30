module Test.Reversi.Board where

import           Data.Matrix      (Matrix)
import qualified Data.Matrix      as Matrix
import           Reversi.Board    as Board
import           Reversi.Coord
import           Reversi.Piece
import           Test.Tasty
import           Test.Tasty.Hspec
import           Test.Tasty.HUnit


sampleMatrix :: Matrix (Maybe Piece)
sampleMatrix = Matrix.fromLists
    [ [n, n, w, w, n, b, n, n]
    , [n, n, n, w, w, b, n, b]
    , [b, w, w, w, b, w, w, b]
    , [n, b, w, b, b, w, w, b]
    , [b, b, b, w, w, b, w, b]
    , [w, n, b, w, w, w, b, b]
    , [n, n, b, w, w, w, n, b]
    , [n, b, b, b, b, b, n, n]
    ]
  where
    (n, b, w) = (Nothing, Just Black, Just White)

sampleBoard :: Board
sampleBoard = Board
  { matrix = sampleMatrix
  , black  = 25
  , white  = 22
  }

scprop_hasCoord :: Coord -> Bool
scprop_hasCoord (x, y) =
  (0 < x && x <= Board.size && 0 < y && y <= Board.size) == Board.hasCoord (x, y)

test_matrixIndex :: [TestTree]
test_matrixIndex =
  [ testCase "board has not piece"         $ mat !!! (1, 4) @?= Nothing
  , testCase "board has white piece"       $ mat !!! (2, 3) @?= Just White
  , testCase "board has black piece"       $ mat !!! (1, 5) @?= Just Black
  , testCase "board has not coord (0, 8)"  $ mat !!! (1, 9) @?= Nothing
  , testCase "board has not coord (-1, 0)" $ mat !!! (0, 1) @?= Nothing
  ]
  where
    mat = matrix sampleBoard

test_getFlip :: [TestTree]
test_getFlip =
    [ testCase "if put white piece on (1, 4) and dir is (0, -1)" $
        Board.getFlip sampleBoard White (1, 4) (0, -1) @?= 0
    , testCase "if put white piece on (1, 4) and dir is (1, 0)" $
        Board.getFlip sampleBoard White (1, 4) (1, 0) @?= 1
    , testCase "if put white piece on (1, 4) and dir is (-1, 0)" $
        Board.getFlip sampleBoard White (1, 4) (1, 1) @?= 2
    , testCase "if put black piece on (5, 1) and dir is (0, -1)" $
        Board.getFlip sampleBoard Black (5, 1) (0, -1) @?= 0
    , testCase "if put black piece on (5, 1) and dir is (1, 0)" $
        Board.getFlip sampleBoard Black (5, 1) (0, 1) @?= 1
    , testCase "if put black piece on (5, 1) and dir is (-1, 0)" $
        Board.getFlip sampleBoard Black (5, 1) (-1, 1) @?= 2
    ]

unit_getMove :: IO ()
unit_getMove = do
  Board.getMove sampleBoard White (pos expected) @?= expected
  where
    expected = Move
      { pos = (1, 4)
      , flips =
          [ 0, 0, 0
          , 0,    1
          , 0, 1, 2
          ]
      }

spec_moves :: Spec
spec_moves =
  describe "Board.move" $
    it "expected list in no order" $
      Board.moves White sampleBoard `shouldMatchList` expected
  where
    expected =
      [ Move
          { pos = (5, 1)
          , flips =
              [ 0, 0, 0
              , 0,    0
              , 0, 0, 1
              ]
          }
      , Move
          { pos = (7, 2)
          , flips =
              [ 0, 0, 0
              , 1,    0
              , 0, 0, 0
              ]
          }
      , Move
          { pos = (1, 4)
          , flips =
              [ 0, 0, 0
              , 0,    1
              , 0, 1, 2
              ]
          }
      , Move
          { pos = (2, 6)
          , flips =
              [ 0, 2, 0
              , 0,    1
              , 0, 0, 0
              ]
          }
      , Move
          { pos = (2, 7)
          , flips =
              [ 0, 0, 1
              , 0,    1
              , 0, 0, 0
              ]
          }
      , Move
          { pos = (7, 7)
          , flips =
              [ 0, 1, 0
              , 0,    0
              , 0, 0, 0
              ]
          }
      ]

unit_doFlip :: IO ()
unit_doFlip =
  Board.doFlip White (1, 4) (1, 1) 2 sampleBoard @?= expected
  where
    expected = Board
      { black = 23
      , white = 24
      , matrix = Matrix.fromLists
          [ [n, n, w, w, n, b, n, n]
          , [n, n, n, w, w, b, n, b]
          , [b, w, w, w, b, w, w, b]
          , [n, b, w, b, b, w, w, b]
          , [b, w, b, w, w, b, w, b]
          , [w, n, w, w, w, w, b, b]
          , [n, n, b, w, w, w, n, b]
          , [n, b, b, b, b, b, n, n]
          ]
      }
    n = Nothing
    b = Just Black
    w = Just White

unit_doMove :: IO ()
unit_doMove =
  Board.doMove White mov sampleBoard @?= expected
  where
    mov = Move
      { pos = (1, 4)
      , flips =
          [ 0, 0, 0
          , 0,    1
          , 0, 1, 2
          ]
      }
    expected = Board
      { black = 21
      , white = 27
      , matrix = Matrix.fromLists
          [ [n, n, w, w, n, b, n, n]
          , [n, n, n, w, w, b, n, b]
          , [b, w, w, w, b, w, w, b]
          , [w, w, w, b, b, w, w, b]
          , [w, w, b, w, w, b, w, b]
          , [w, n, w, w, w, w, b, b]
          , [n, n, b, w, w, w, n, b]
          , [n, b, b, b, b, b, n, n]
          ]
      }
    (n, b, w) = (Nothing, Just Black, Just White)
