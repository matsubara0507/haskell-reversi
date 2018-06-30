module Main where

import qualified Reversi.Game    as Game
import           Reversi.Players (AlphaBetaPlayer (..), Human (..))

main :: IO ()
main = Game.start (AlphaBetaPlayer 7) Human
