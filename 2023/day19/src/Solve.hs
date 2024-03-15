module Solve where

import Parse
import qualified Data.Map
import Data.Foldable (Foldable(foldl'))

import Debug.Trace (trace)

data PartState = Moved !Part | Unprocessed !Part deriving Show

applyInstructions :: [Instruction] -> Part -> PartState
applyInstructions instructions init = foldl' applyInstruction (Unprocessed init) instructions
    where applyInstruction (Unprocessed p) (Move pred dest) = if pred p then Moved (Part dest (getX p) (getM p) (getA p) (getS p)) else Unprocessed p
          applyInstruction state _ = state

sumVals :: [Part] -> Int
sumVals = sum . map (\p -> getX p + getM p +  getA p + getS p)

findAcceptedParts :: [Part] -> Workflows -> [Part]
findAcceptedParts [] _ = []
findAcceptedParts (part:parts) workflows =
    case newPos of
        "A" -> part : findAcceptedParts parts workflows
        "R" -> findAcceptedParts parts workflows
        _ -> findAcceptedParts (newPart:parts) workflows
    where (Moved newPart) = case Data.Map.lookup (getPos part) workflows of
                    Nothing -> error $ "Incorrect position of part " ++ show (getPos part)
                    Just instructions -> applyInstructions instructions part
          newPos = getPos newPart
