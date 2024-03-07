{-# LANGUAGE FlexibleInstances #-}
module Parse where

import Data.Map (Map)
import qualified Data.Map as Map

data Tile =
    Empty
    | FMirror
    | BMirror
    | HSplit
    | VSplit
    deriving Eq

instance Show Tile where
    show Empty = "."
    show FMirror = "/"
    show BMirror = "\\"
    show HSplit = "-"
    show VSplit = "|"

translateTile :: Tile -> Char
translateTile Empty = '.'
translateTile FMirror = '/'
translateTile BMirror = '\\'
translateTile HSplit = '-'
translateTile VSplit = '|'

translateInput :: Char -> Tile
translateInput '/' = FMirror
translateInput '\\' = BMirror
translateInput '-' = HSplit
translateInput '|' = VSplit
translateInput _ = Empty

type Grid = Map (Int, Int) Tile

arraySize :: [[a]] -> (Int, Int)
arraySize xss = (length xss, (length . head) xss)

arrayToGrid :: [[Tile]] -> ((Int, Int), Grid)
arrayToGrid xs = (arraySize xs, Map.fromList keyIndices)
    where keyIndices = concat $ zipWith mergeIndices [0..] $ map (zip [0..]) xs
          mergeIndices i = map (\(j, x) -> ((i, j), x))

parseInput :: String -> IO ((Int, Int), Grid)
parseInput fileName = do
    content <- readFile fileName
    return (arrayToGrid . map (map translateInput) $ lines content)
