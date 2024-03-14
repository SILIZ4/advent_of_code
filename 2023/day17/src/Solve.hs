module Solve where

import Data.Matrix (unsafeGet, nrows, ncols)
import Data.Maybe (fromJust, catMaybes, isJust)
import Data.PSQueue (PSQ, Binding ((:->)))
import Parse (Grid)
import qualified Data.PSQueue
import Data.List (foldl')


data Direction = North | South | East | West deriving (Eq, Show)
data Orientation = Vertical | Horizontal deriving (Eq, Ord, Show)
data Move = Move !(Int, Int) !Direction deriving Show
type Coord = (Int, Int)

fromDirection :: Direction -> Orientation
fromDirection North = Vertical
fromDirection South = Vertical
fromDirection East = Horizontal
fromDirection West = Horizontal

inRange :: (Int, Int) -> (Int, Int) -> Bool
inRange (i, j) (n, m) = i>=1 && i<=n && j>=1 && j<=m

enumNeighbours :: (Int, Int) -> [Int] -> Orientation -> Coord -> [Move]
enumNeighbours coord increments orient size =
    concatMap (catMaybes . moveUntilNothing) newDirs
    where moveUntilNothing dir = takeWhile isJust [createMove (newIndices dir coord steps) dir | steps <- increments]
          createMove (i', j') newDir = if inRange (i', j') size then
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

dijkstra1 :: Coord -> Grid -> PSQ (Coord, Orientation) Int -> Int
dijkstra1 dest grid heap
    | coord == dest = cost
    | otherwise = dijkstra1 dest grid newHeap

    where ((coord, orient) :-> cost, strippedHeap) = fromJust $ Data.PSQueue.minView heap
          (newHeap, _, _) = foldl' updateCell (strippedHeap, cost, cost) neighbours
          neighbours = enumNeighbours coord [1..3] orient (nrows grid, ncols grid)

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


dijkstra2 :: Coord -> Grid -> PSQ (Coord, Orientation) Int -> Int
dijkstra2 dest grid heap
    | coord == dest = cost
    | otherwise = dijkstra2 dest grid newHeap

    where ((coord, orient) :-> cost, strippedHeap) = fromJust $ Data.PSQueue.minView heap
          (newHeap, _, _) = foldl' updateCell (strippedHeap, initCostL, initCostR) neighbours

          gridSize = (nrows grid, ncols grid)
          (initCostL, initCostR) =
                let sumCosts inc = foldl' (\acc x-> acc + getCost (inc x)) cost [1..3]
                    getCost (i', j') = if inRange (i', j') gridSize then unsafeGet i' j' grid else 0
                    (i, j) = coord
                in
                case orient of -- Must be coherent with leftVal
                    Horizontal -> (sumCosts (\x -> (i-x, j)), sumCosts (\x -> (i+x, j)))
                    Vertical   -> (sumCosts (\x -> (i, j-x)), sumCosts (\x -> (i, j+x)))

          neighbours = enumNeighbours coord [4..10] orient gridSize

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
