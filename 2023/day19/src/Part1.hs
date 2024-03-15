module Main where

import System.Environment (getArgs)

import Parse
import Solve

main :: IO ()
main = do
    args <- getArgs
    (workflows, parts) <- parseInput $ head args
    print $ sumVals $ findAcceptedParts parts workflows
