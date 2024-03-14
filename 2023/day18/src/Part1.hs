module Main where

import System.Environment (getArgs)
import Parse
import Solve

dropLast :: (a, b, c) -> (a, b)
dropLast (x, y, z) = (x, y)

main :: IO ()
main = do
    args <- getArgs
    puzzle <- parseInput (head args)
    print $ (area . toCoords (0, 0) . map dropLast) puzzle
