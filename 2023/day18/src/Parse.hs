module Parse where

import Numeric (readHex)


data Direction = R | D | L | U deriving Show

dirFromChar :: Char -> Direction
dirFromChar 'R' = R
dirFromChar 'D' = D
dirFromChar 'L' = L
dirFromChar 'U' = U

dirFromChar '0' = R
dirFromChar '1' = D
dirFromChar '2' = L
dirFromChar '3' = U

instructionFromColor :: String -> (Direction, Integer)
instructionFromColor (c:cs) = (dirFromChar (last cs), (fst . head . readHex . take 5) cs)

parseLine :: String -> (Direction, Int, String)
parseLine (x:(_:xs)) = (dirFromChar x, read digits, colorCode)
    where digits = takeWhile (/= ' ') xs
          colorCode = takeWhile (/=')') $ dropWhile (/='#') xs

parseInput :: String -> IO [(Direction, Int, String)]
parseInput fileName =
    (fmap . fmap) parseLine $ lines <$> readFile fileName
