module Main where

import Parse (parseInput)
import Solve
import qualified Data.Set as Set
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    (size, pattern) <- parseInput (head args)
    print $ (countEnergized size . initPuzzle) pattern
