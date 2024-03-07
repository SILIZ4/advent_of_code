module Solve (initPuzzle, countEnergized) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Parse

import Debug.Trace (trace)
import Data.Foldable (Foldable(toList))

data Direction =
    North
    | South
    | East
    | West
    deriving (Eq, Ord, Show)

data Beam =
    Stopped !(Int, Int)
    | Running !(Int, Int) !Direction
    deriving (Show)

data Puzzle =
    Puzzle {
        getBeams :: ![Beam],
        getGrid :: !Grid,
        getEnergized :: !(Set.Set (Int, Int, Direction))
    }

isBeamReflected :: Beam -> Tile -> Bool
isBeamReflected (Stopped _) _ = True
isBeamReflected _ FMirror = True
isBeamReflected _ BMirror = True
isBeamReflected (Running _ North) HSplit = True
isBeamReflected (Running _ South) HSplit = True
isBeamReflected (Running _ East) VSplit = True
isBeamReflected (Running _ West) VSplit = True
isBeamReflected _ _ = False

beamNextStop :: Beam -> Grid -> Beam
beamNextStop (Stopped c) _ = Stopped c
beamNextStop beam@(Running (i, j) dir) grid =
    case Map.lookup newCoord grid of
        Nothing -> Stopped (i, j)
        (Just nextTile) ->
            if isBeamReflected beam nextTile then
                newBeam
            else
                beamNextStop newBeam grid
    where newBeam = Running newCoord dir
          newCoord = case dir of
            North -> (i-1, j)
            South -> (i+1, j)
            East -> (i, j+1)
            West -> (i, j-1)

-- The following arbitrary convention is chosen:
--   - horizontal splits are redirected to the East and duplicated to the West;
--   - vertical splits are redirected to the North and duplicated to the South.
redirectBeam :: Beam -> Tile -> Beam
redirectBeam (Running c North) FMirror = Running c East
redirectBeam (Running c North) BMirror = Running c West
redirectBeam (Running c North) HSplit = Running c East

redirectBeam (Running c South) FMirror = Running c West
redirectBeam (Running c South) BMirror = Running c East
redirectBeam (Running c South) HSplit = Running c East

redirectBeam (Running c East) FMirror = Running c North
redirectBeam (Running c East) BMirror = Running c South
redirectBeam (Running c East) VSplit = Running c North

redirectBeam (Running c West) FMirror = Running c South
redirectBeam (Running c West) BMirror = Running c North
redirectBeam (Running c West) VSplit = Running c North

redirectBeam (Running c _) _ = Stopped c
redirectBeam b@(Stopped _) _ = b


duplicateBeam :: Beam -> Tile -> Maybe Beam
duplicateBeam (Running c North) HSplit = Just (Running c West)
duplicateBeam (Running c South) HSplit = Just (Running c West)
duplicateBeam (Running c East) VSplit = Just (Running c South)
duplicateBeam (Running c West) VSplit = Just (Running c South)
duplicateBeam _ _ = Nothing


removeDuplicateTiles ::  (Int, Int) -> Set.Set (Int, Int, Direction) -> Set.Set (Int, Int)
removeDuplicateTiles (n, m) = Set.fromList . filter (\(x, y) -> x < n && y < m) . map (\(i, j, _) -> (i, j)) . toList

countEnergized :: (Int, Int) -> Puzzle -> Int
countEnergized (n, m) = Set.size . removeDuplicateTiles (n, m) . getEnergized . solve

getInitBeam :: (Int, Int) -> Grid -> Beam
getInitBeam (i, j) grid = case redirectBeam beam initTile of
                            (Stopped _) -> beam
                            redirected -> redirected
                          where beam = Running (i, j) East
                                initTile = fromJust $ Map.lookup (i, j) grid

initPuzzle :: Grid -> Puzzle
initPuzzle grid = Puzzle [getInitBeam (0, 0) grid] grid Set.empty

solve :: Puzzle -> Puzzle
solve p@(Puzzle [] _ _) = p
solve (Puzzle ((Stopped _): bs) g seen) = solve (Puzzle bs g seen)
solve (Puzzle (beam@(Running (i, j) dir): bs) grid seen) =
    solve (Puzzle newBeams grid (Set.union seen newSeen))
    where propagatedBeam = beamNextStop beam grid
          (i', j') = case propagatedBeam of
                Stopped (x, y) ->  (x, y)
                Running (x, y) _ -> (x, y)
          newTile = fromJust (Map.lookup (i', j') grid)
          newBeam = redirectBeam propagatedBeam newTile
          dupedBeam = duplicateBeam propagatedBeam newTile
          newBeams = case propagatedBeam of
                        Stopped _ -> bs
                        Running _ _ -> if (i', j', dir) `Set.member` seen then
                                bs -- There is a cycle
                            else
                                case dupedBeam of
                                    Nothing -> newBeam : bs
                                    Just dup -> newBeam : (dup: bs)
          newSeen = Set.fromList [(r, s, dir) | r <- [min iAdjusted i'..max iAdjusted i'],
                                                s <- [min jAdjusted j'..max jAdjusted j']]
          -- These shifts add out-of-bound tiles when beam is redirected on the boundary
          iAdjusted = case dir of
                        North -> i - 1
                        South -> i + 1
                        _ -> i
          jAdjusted = case dir of
                        East -> j + 1
                        West -> j - 1
                        _ -> j
