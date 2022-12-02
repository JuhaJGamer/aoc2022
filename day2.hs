module Day2 where 

import System.IO
import Data.Bifunctor

data Shape = Rock | Paper | Scissors
    deriving (Eq, Show)

data Result = Win | Draw | Loss
    deriving Show

type Round = [(Shape, Shape)]

shapeScore :: Shape -> Int
shapeScore Rock     = 1
shapeScore Paper    = 2
shapeScore Scissors = 3

score :: Result -> Int
score Win = 6
score Draw = 3
score Loss = 0

roundResult :: Shape -> Shape -> Result
roundResult Rock Scissors  = Win
roundResult Paper Rock     = Win
roundResult Scissors Paper = Win
roundResult x y
  | x == y    = Draw 
  | otherwise = Loss

fullScore :: Shape -> Shape -> Int
fullScore x y = shapeScore x + score (roundResult x y)

readShape :: Char -> Shape 
readShape 'A' = Rock
readShape 'X' = Rock
readShape 'B' = Paper
readShape 'Y' = Paper
readShape 'C' = Scissors
readShape 'Z' = Scissors

readResult :: Char -> Result
readResult 'Z' = Win
readResult 'Y' = Draw
readResult 'X' = Loss

winning :: Shape -> Shape
winning Rock     = Paper
winning Paper    = Scissors
winning Scissors = Rock

losing :: Shape -> Shape
losing Rock     = Scissors
losing Paper    = Rock
losing Scissors = Paper

move :: Shape -> Result -> Shape
move x Draw = x
move x Win  = winning x
move x Loss = losing x

main = do inputLines <- lines <$> getContents
          let input = map (\[x, ' ', z] -> (x, z)) inputLines
              rounds1 = map (bimap readShape readShape) input
              strats2 = map (bimap readShape readResult) input
              rounds2 = zipWith (curry $ second $ uncurry move) (map fst strats2) strats2
              results1 = map (uncurry $ flip fullScore) rounds1 
              results2 = map (uncurry $ flip fullScore) rounds2
          print $ sum results1
          print $ sum results2
          putStrLn "trolled"
