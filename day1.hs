module Day1 where

import System.IO
import Data.List.Split
import Data.List
import Data.Ord


getElves :: [String] -> [[Int]]
getElves = map (map read) . splitOn [""]


main = do input <- lines <$> getContents
          let elves = getElves input       
              greatElf = maximumBy (comparing sum) elves
          print $ sum (greatElf :: [Int])
          putStrLn "trolled"
