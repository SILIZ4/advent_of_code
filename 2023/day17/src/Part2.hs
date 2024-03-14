module Main where

import System.Environment (getArgs)

import Data.Matrix (nrows, ncols)
import Parse
import Solve
import Data.PSQueue (singleton)

main :: IO ()
main = do
    args <- getArgs
    grid <- parseInput (head args)
    let size = (nrows grid, ncols grid) in
        print $ dijkstra2 size grid (initHeap size)
