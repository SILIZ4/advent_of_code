module Solve where

import Parse
import qualified Data.Map
import Data.Foldable (Foldable(foldl'))

import Debug.Trace (trace)

data State a = Processed !a | Unprocessed !a deriving Show

applyInstructions :: [Instruction] -> Part -> State Part
applyInstructions instructions init = foldl' applyInstruction (Unprocessed init) instructions
    where applyInstruction (Unprocessed p) (Move pred dest) = if pred p then Processed (Part dest (getX p) (getM p) (getA p) (getS p)) else Unprocessed p
          applyInstruction state _ = state

sumVals :: [Part] -> Int
sumVals = sum . map (\p -> getX p + getM p +  getA p + getS p)

findAcceptedParts :: [Part] -> Workflows Instruction -> [Part]
findAcceptedParts [] _ = []
findAcceptedParts (part:parts) workflows =
    case newPos of
        "A" -> part : findAcceptedParts parts workflows
        "R" -> findAcceptedParts parts workflows
        _ -> findAcceptedParts (newPart:parts) workflows
    where (Processed newPart) = case Data.Map.lookup (getPos part) workflows of
                    Nothing -> error $ "Incorrect position of part " ++ show (getPos part)
                    Just instructions -> applyInstructions instructions part
          newPos = getPos newPart


applySplitInstructions :: [SplitInstruction] -> PartRange -> State [PartRange]
applySplitInstructions instructions init = foldl' applyInstruction (Unprocessed [init]) instructions
    where applyInstruction (Processed ps) _ = Processed ps
          applyInstruction (Unprocessed (p@(PartRange ranges origin):ps)) (Split splitfunc dest) = case splitfunc p of
            (Just moved, Nothing) -> Processed (PartRange moved dest : ps)
            (Just moved, Just notmoved) -> Unprocessed ( PartRange notmoved origin : (PartRange moved dest : ps))
            (Nothing, Just notmoved) -> Unprocessed ( PartRange notmoved origin : ps )


rangelen :: (Int, Int) -> Integer
rangelen (x, y) = 1+fromIntegral y - fromIntegral x

countPossibilities :: [PartRange] -> Integer
countPossibilities = foldl' (\acc (PartRange (Ranges x m a s) _)  -> acc + rangelen x * rangelen m * rangelen a * rangelen s) 0

findRangeAcceptedParts :: [PartRange] -> Workflows SplitInstruction -> [PartRange]
findRangeAcceptedParts [] _ = []
findRangeAcceptedParts (part@(PartRange _ "A"):parts) workflows = part : findRangeAcceptedParts parts workflows
findRangeAcceptedParts (part@(PartRange _ "R"):parts) workflows = findRangeAcceptedParts parts workflows
findRangeAcceptedParts (part@(PartRange _ origin):parts) workflows =
    findRangeAcceptedParts (newParts ++ parts) workflows
    where (Processed newParts) = case Data.Map.lookup origin workflows of
                        Nothing -> error $ "Incorrect position of part " ++ show origin
                        Just instructions -> applySplitInstructions instructions part
