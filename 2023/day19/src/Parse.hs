module Parse where

import Data.Char (isDigit)
import Data.Map (Map)
import qualified Data.Map

import Debug.Trace (trace)


data Instruction = Move !(Part->Bool) !String
type Workflows a = Map String [a]

data Part = Part {
    getPos :: !String,
    getX :: !Int,
    getM :: !Int,
    getA :: !Int,
    getS :: !Int
}
    deriving Show

splitWhen :: (Char -> Bool) -> String -> [String]
splitWhen pred = foldr (\x (ys:yss) -> if pred x then []:(ys:yss) else (x:ys):yss) [[]]

parseInstructions :: [String] -> [Instruction]
parseInstructions [] = []
parseInstructions [cs] = [Move (const True) (takeWhile (/='}') cs)]
parseInstructions (cs:css) = f : parseInstructions css
    where f = Move (\part -> val part `op` threshold) dest
          val = case head cs of
                    'x' -> getX
                    'm' -> getM
                    'a' -> getA
                    's' -> getS
          op = if cs !! 1 == '<' then (<) else (>)
          threshold = (read . takeWhile isDigit . drop 2) cs
          dest = (drop 1 . dropWhile (/=':')) cs


parseSplitInstructions :: [String] -> [SplitInstruction]
parseSplitInstructions [] = []
parseSplitInstructions [cs] = [Split (\(PartRange r _) -> (Just r, Nothing)) (takeWhile (/='}') cs)]
parseSplitInstructions (cs:css) = Split f dest: parseSplitInstructions css
    where f (PartRange ranges@(Ranges x m a s) _) =
            let var = head cs
                createranges r = case var of
                            'x' -> Ranges r m a s
                            'm' -> Ranges x r a s
                            'a' -> Ranges x m r s
                            's' -> Ranges x m a r
                range@(start, end) = case var of
                            'm' -> m; 'x' -> x; 'a' -> a; 's' -> s
                in
                if cs !! 1 == '<' then
                    if threshold > end then
                        (Just ranges, Nothing)
                    else
                        if threshold > start then
                            (Just (createranges (start, threshold-1)), Just (createranges (threshold, end)))
                        else
                            (Nothing, Just ranges)
                else
                    if threshold < start then
                        (Just ranges, Nothing)
                    else
                        if threshold < end then
                            (Just (createranges (threshold+1, end)), Just (createranges (start, threshold)))
                        else
                            (Nothing, Just ranges)
          threshold = (read . takeWhile isDigit . drop 2) cs
          dest = (drop 1 . dropWhile (/=':')) cs


-- PartRange -> (moved, not moved)
data SplitInstruction = Split !(PartRange->(Maybe Ranges, Maybe Ranges)) !String
data PartRange = PartRange !Ranges !String deriving Show
data Ranges = Ranges !(Int, Int) !(Int, Int) !(Int, Int) !(Int, Int) deriving Show

workflowFromStr :: String -> (String,  [Instruction])
workflowFromStr xs = (name, (parseInstructions . splitWhen (==',')) (workflowStr xs))
     where name = takeWhile (/='{') xs
           workflowStr = drop 1 . dropWhile (/='{')

workflowRangeFromStr :: String -> (String,  [SplitInstruction])
workflowRangeFromStr xs = (name, (parseSplitInstructions . splitWhen (==',')) (workflowStr xs))
     where name = takeWhile (/='{') xs
           workflowStr = drop 1 . dropWhile (/='{')


partFromStr :: String -> Part
partFromStr str = Part "in" (read x) (read m) (read a) (read s)
    where (x, xEnd) = nextDigit str
          (m, mEnd) = nextDigit xEnd
          (a, aEnd) = nextDigit mEnd
          (s, _) = nextDigit aEnd
          nextDigit cs = let beg = dropWhile (not . isDigit) cs
                             end = dropWhile (/=',') beg
                         in (takeWhile isDigit beg, end)


biApply :: (a->b) -> (c->d) -> (a, c) -> (b, d)
biApply f g (x, y) = (f x, g y)


parseInput :: String -> IO (Workflows Instruction, [Part])
parseInput fileName =
    biApply (Data.Map.fromList . map workflowFromStr) (map partFromStr . drop 1) . break (=="") . lines <$> readFile fileName

parseRangeInput :: String -> IO (Workflows SplitInstruction)
parseRangeInput fileName =
    Data.Map.fromList . map workflowRangeFromStr . takeWhile (/= "") . lines <$> readFile fileName
