module Main where

import System.Environment (getArgs)
import Parse
import Solve


main :: IO ()
main = do
    args <- getArgs
    puzzle <- parseInput (head args)
    print $ (area . toCoords (0, 0) . map (\(x, y, z) -> instructionFromColor z)) puzzle
