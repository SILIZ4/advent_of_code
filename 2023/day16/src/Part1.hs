module Main where

import qualified Data.Set as Set
import System.Environment (getArgs)

import Parse
import Solve

main :: IO ()
main = do
    args <- getArgs
    (size, pattern) <- parseInput (head args)
    --print $ (countEnergized size . initPuzzle (Running (0, 3) South)) pattern
    print $ (countEnergized size . initPuzzle (Running (0, 0) East)) pattern
