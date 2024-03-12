module Solve where

import Data.Matrix (unsafeGet, nrows, ncols)
import Data.Maybe (fromJust, catMaybes)
import Data.PSQueue (PSQ, Binding ((:->)))
import Parse (Grid)
import qualified Data.PSQueue
import Data.List (foldl')

import Debug.Trace (trace)


data Direction = North | South | East | West deriving (Eq, Show)
data Orientation = Vertical | Horizontal deriving (Eq, Ord, Show)
data Move = Move !(Int, Int) !Direction deriving Show

instance Eq Move where
    (Move coord _) == (Move coord' _) = coord == coord'

instance Ord Move where
    compare (Move coord _) (Move coord' _) = compare coord coord'

type Coord = (Int, Int)

fromDirection :: Direction -> Orientation
fromDirection North = Vertical
fromDirection South = Vertical
fromDirection East = Horizontal
fromDirection West = Horizontal

inRange :: (Int, Int) -> (Int, Int) -> Bool
inRange (i, j) (n, m) = i>=1 && i<=n && j>=1 && j<=m

enumNeighbours :: (Int, Int) -> Orientation -> Coord -> [Move]
enumNeighbours coord orient size =
    catMaybes [createMove (newIndices dir coord steps) dir | dir <- newDirs, steps <- [1..3]]
    where createMove (i', j') newDir = if inRange (i', j') size then
                  Just $ Move (i', j') newDir
              else
                  Nothing
          newDirs = case orient of
            Vertical   -> [East, West]
            Horizontal -> [North, South]
          newIndices newDir (i', j') steps  = case newDir of
            North -> (i'-steps, j')
            South -> (i'+steps, j')
            East  -> (i', j'+steps)
            West  -> (i', j'-steps)

initHeap :: (Int, Int) -> PSQ (Coord, Orientation) Int
initHeap (rows, cols) = Data.PSQueue.insert ((1, 1), Vertical) 0 $ Data.PSQueue.insert ((1, 1), Horizontal) 0 $ Data.PSQueue.fromList
                        [((i, j), orient) :-> 1000000 | i <- [1..rows], j <- [1..cols], i/=1 || j/=1,
                                                        orient <- [Horizontal, Vertical]]

astar :: Coord -> Grid -> PSQ (Coord, Orientation) Int -> Int
astar dest grid heap
    | coord == dest = cost
    | otherwise = astar dest grid newHeap

    where ((coord, orient) :-> cost, strippedHeap) = fromJust $ Data.PSQueue.minView heap
          (newHeap, _, _) = foldl' updateCell (strippedHeap, cost, cost) neighbours
          neighbours = enumNeighbours coord orient (nrows grid, ncols grid)

          updateCell (h, costL, costR) (Move coord' dir') =
            case Data.PSQueue.lookup (coord', orient') h of
                Nothing -> (h, costL', costR')
                Just currentCost -> if cumCost < currentCost then
                            (Data.PSQueue.insert (coord', orient') cumCost h, costL', costR')
                        else
                            (h, costL', costR')
            where orient' = fromDirection dir'
                  costL' = if leftVal then costL + cost' else costL
                  costR' = if leftVal then costR else costR + cost'
                  cumCost = if leftVal then costL' else costR'
                  cost' = unsafeGet i' j' grid
                  (i', j') = coord'
                  leftVal = dir' == North || dir' == West -- Arbitrary, must check one vertical and horizontal
