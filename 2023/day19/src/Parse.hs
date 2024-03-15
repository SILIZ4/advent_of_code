module Parse where

import Data.Char (isDigit)
import Data.Map (Map)
import qualified Data.Map


data Instruction = Move !(Part->Bool) !String
type Workflows = Map String [Instruction]

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


workflowFromStr :: String -> (String,  [Instruction])
workflowFromStr xs = (name, (parseInstructions . splitWhen (==',')) (workflowStr xs))
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


parseInput :: String -> IO (Workflows, [Part])
parseInput fileName =
    biApply (Data.Map.fromList . map workflowFromStr) (map partFromStr . drop 1) . break (=="") . lines <$> readFile fileName
