module Main where

import qualified Data.Set as Set
import System.Environment (getArgs)

import Parse
import Solve
import Data.Foldable (Foldable(foldl'))

main :: IO ()
main = do
    args <- getArgs
    (size, pattern) <- parseInput (head args)
    print $ foldl' (\acc b -> max acc (res size b pattern)) 0 $ beams size
    where res size beam = countEnergized size . initPuzzle beam
          beams (n, m) =
            [Running (i, 0) East | i <- [0..n-1]]
            ++ [Running (0, j) South | j <- [0..m-1]]
            ++ [Running (i, m-1) West | i <- [0..n-1]]
            ++ [Running (n-1, j) North | j <- [0..m-1]]
