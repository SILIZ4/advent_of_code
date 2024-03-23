module Parse where

import Data.Map (Map)
import qualified Data.Map
import Data.Sequence (Seq)
import qualified Data.Sequence


data PulseIntensity = Low | High deriving (Eq, Show)
data Pulse = Pulse !PulseIntensity !String !String deriving (Eq, Show)
data Module = Conjunction !(Map String PulseIntensity) ![String] | FlipFlop !Bool ![String] deriving (Eq, Show)

type Modules = Map String Module

getNeighbours :: Module -> [String]
getNeighbours (Conjunction _ ns) = ns
getNeighbours (FlipFlop _ ns) = ns

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen cond = foldr (\x (ys:yss) -> if cond x then []:(ys:yss) else (x:ys):yss) [[]]

-- Returns module or broadcast init
moduleFromLine :: String -> Either (String, Module) [String]
moduleFromLine (c:cs) = case c of
    '%' -> Left (name, FlipFlop False neighbours)
    '&' -> Left (name, Conjunction Data.Map.empty neighbours)
    _   -> Right neighbours
    where name = takeWhile (/=' ') cs
          neighbours = (map (drop 1) . splitWhen (==',') . drop 1 . dropWhile (/='>')) cs
moduleFromLine [] = error "Empty line"


initConjunctions :: Map String Module -> Map String Module
initConjunctions modules = foldr addNeighbours modules (Data.Map.toList modules)
    where addNeighbours (name, m) ms =
            let addNeighbour n innerms =
                    case Data.Map.lookup n innerms of
                            Nothing -> innerms
                            Just (FlipFlop _ _) -> innerms
                            Just (Conjunction current conNeighbours) ->
                                Data.Map.insert n (Conjunction (Data.Map.insert name Low current) conNeighbours) innerms
            in foldr addNeighbour ms (getNeighbours m)


parseInput :: String -> IO (Modules, Seq Pulse)
parseInput fileName =
    initConj . foldr (initMap . moduleFromLine) (Data.Map.empty, Data.Sequence.empty). lines <$> readFile fileName
    where initMap x (modules, inits) = case x of
            Right xs -> (modules, Data.Sequence.fromList (map (Pulse Low "broadcaster") xs))
            Left (name, m) -> (Data.Map.insert name m modules, inits)
          initConj (ms, inits) = (initConjunctions ms, inits)
