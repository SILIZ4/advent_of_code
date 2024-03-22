module Main where

import System.Environment (getArgs)

import Parse
import Solve

main :: IO ()
main = do
    args <- getArgs
    workflows <- parseRangeInput $ head args
    print $ countPossibilities $ findRangeAcceptedParts [PartRange (Ranges (1, 4000) (1, 4000) (1, 4000) (1, 4000)) "in"] workflows
