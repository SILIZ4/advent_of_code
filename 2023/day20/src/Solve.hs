module Solve where

import qualified Data.Map
import Data.Sequence (Seq ((:<|), (:|>)), (><))
import qualified Data.Sequence
import Parse


process :: Module -> Pulse -> Maybe (Module, PulseIntensity)
process (FlipFlop _ _) (Pulse High _ _) = Nothing
process (FlipFlop False ns) _ = Just (FlipFlop True ns, High)
process (FlipFlop True ns) _ = Just (FlipFlop False ns, Low)
process (Conjunction bs ns) (Pulse pulse from _) = if all ((==High) . snd) $ Data.Map.toList newbs then
        Just (newCon, Low)
    else
        Just (newCon, High)
    where newbs = Data.Map.insert from pulse bs
          newCon = Conjunction newbs ns


findEl :: Eq a => (a->Bool) -> Seq a -> Maybe (a, Int)
findEl _ Data.Sequence.Empty = Nothing
findEl cond (x:<|xs) = if cond x then Just (x, 0) else fmap (+1) <$> findEl cond xs

biApply :: (a->a) -> (a, a)-> (a, a)
biApply f (x, y)  = (f x, f y)

mergeTuple :: (a->a->a) -> (a, a) -> (a, a) -> (a, a)
mergeTuple f (x, y) (x', y') = (f x x', f y y')

pressN :: Seq Pulse -> Int -> Seq (Modules, (Int, Int)) -> (Int, Int)
pressN _ _ Data.Sequence.Empty = (0, 0)
pressN _ 0 (_:|>(_, counts)) = counts
pressN inits presses seen@(ps:|>(modules, counts))= case cyclePress of
            Nothing -> pressN inits (presses-1) (seen:|>(newModules, newCounts))
            Just ((_, prevCounts), i) -> let
                  cycleIncrement = mergeTuple (-) counts prevCounts
                  cycleLength = Data.Sequence.length seen - i - 1
                  (cycles, r) = presses `divMod` cycleLength
                  remainder = snd $ Data.Sequence.index seen (i+r)

              in mergeTuple (+) remainder $ mergeTuple (+) counts $ biApply (*cycles) cycleIncrement

        where (newModules, newCounts) = pressButton inits modules $ mergeTuple (+) counts (1, 0)
              cyclePress = findEl ((==modules) . fst) ps

pressButton :: Seq Pulse -> Modules -> (Int, Int) -> (Modules, (Int, Int))
pressButton Data.Sequence.Empty ms counts = (ms, counts)
pressButton (p@(Pulse intensity _ dest) :<| ps) ms (c, c') = case newVals of
                Nothing -> pressButton ps ms newCounts
                Just (newms, newps) -> pressButton (ps >< newps) newms newCounts
    where newCounts = case intensity of
                                Low -> (c+1, c')
                                High -> (c, c'+1)
          newVals = case fmap (`process` p) destModule of
                Nothing -> Nothing
                Just Nothing -> Nothing
                Just (Just (m, newIntensity)) ->
                    Just (Data.Map.insert dest m ms, Data.Sequence.fromList (map (Pulse newIntensity dest) (getNeighbours m)))
          destModule = Data.Map.lookup dest ms
