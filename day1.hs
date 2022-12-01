module Day1 where

import System.IO
import Data.List.Split
import Data.List
import Data.Ord


getElves :: [String] -> [[Int]]
getElves = map (map read) . splitOn [""]

greatElf = maximumBy (comparing sum)

nGreatElves :: Int -> [[Int]] -> [[Int]]
nGreatElves _ [] = undefined
nGreatElves 0 _  = []
nGreatElves n xs = curGreatElf : nGreatElves (n-1) [x | x <- xs, x /= curGreatElf]
    where curGreatElf = greatElf xs


main = do input <- lines <$> getContents
          let elves = getElves input       
          print $ sum (greatElf elves)
          (print . sum . map sum) $ nGreatElves 3 elves
          putStrLn "trolled"
