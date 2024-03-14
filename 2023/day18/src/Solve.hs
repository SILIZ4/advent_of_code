module Solve where

import Parse
import Data.Array (Array, bounds, listArray, (!))
import Data.Foldable (Foldable(foldl'))


toCoords :: Integral a => (a, a) -> [(Direction, a)] -> Array Int (a, a)
toCoords init xs = listArray (1, length xs) $ drop 1 $ reverse $ foldl' (\ps@(p:_) x -> move p x: ps) [init] xs
    where move (i, j) (dir, n) = case dir of
            U -> (i-n, j)
            D -> (i+n, j)
            R -> (i, j+n)
            L -> (i, j-n)

manhattan :: Integral a => (a, a) -> (a, a) -> a
manhattan (i, j) (i', j') = (max i i' - min i i') + (max j j' - min j j')

area :: Integral a => Array Int (a, a) -> a
area ps = abs a `div` 2 + pathLen `div` 2 + 1 -- Not sure why +1, but it seems to work
    where (a, pathLen) = foldl' addPoint (0, 0) [1..n]
          addPoint (total, len) i =
            (total + x i * (y (i+1) - y (i-1)), len+manhattan (xy i) (xy (i+1)))

          n = (snd . bounds) ps
          xy i
            | i == 0 = ps ! n
            | i == n+1 = ps ! 1
            | otherwise = ps ! i
          x = fst . xy
          y = snd . xy
