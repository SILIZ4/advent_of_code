module Main where

import Parse
import Solve
import Data.Sequence (fromList)
import System.Environment (getArgs)

prod :: (Int, Int) -> Integer
prod (x, y) = fromIntegral x *fromIntegral y

main :: IO ()
main = do
    args <- getArgs
    (modules, inits) <- parseInput $ head args
    print $ prod $ pressN inits 1000 (fromList [(modules, (0, 0))])
