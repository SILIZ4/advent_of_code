module Parse where

import qualified Data.Matrix
import Data.Char (digitToInt)

type Grid = Data.Matrix.Matrix Int

buildMatrix :: [[Int]] -> Grid
buildMatrix xss = Data.Matrix.matrix n m (\(i, j) -> (xss !! (i-1)) !! (j-1))
                    where n = length xss
                          m = (length . head) xss

parseInput :: String -> IO Grid
parseInput fileName =
    buildMatrix . (map . map) digitToInt . lines <$> readFile fileName
