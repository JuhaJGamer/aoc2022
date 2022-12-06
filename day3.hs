module Day3 where

import           Data.Char
import           Data.Functor
import           Data.List
import System.IO

type Item     = Char
type Rucksack = ([Item], [Item])

splitRucksack :: String -> Rucksack
splitRucksack s 
  = let lhalf  = length s `div` 2
        first  = take lhalf s 
        second = drop lhalf s
     in (first, second)

findRepeat :: Rucksack -> [Item]
findRepeat (first, second) = nub [c | c <- first, c `elem` second]

priority  :: Item -> Int
priority c
  | 'a' <= c && c <= 'z' = ord c - ord 'a' + 1
  | 'A' <= c && c <= 'Z' = ord c - ord 'A' + 27
  | otherwise = undefined

triplets :: [a] -> [(a,a,a)]
triplets (x:y:z:xs) = (x,y,z):triplets xs
triplets _          = []

groupBadge :: (Rucksack, Rucksack, Rucksack) -> Item
groupBadge ((ax,ay),(bx,by),(cx,cy))
  = head [c | c <- ax ++ ay, c `elem` (bx ++ by), c `elem` (cx ++ cy)]

main = do rucksacks <- map splitRucksack . lines <$> getContents
          let repeatItems      = rucksacks >>= findRepeat
              repeatPriorities = map priority repeatItems
              groups = triplets rucksacks
          print $ sum $ map (priority . groupBadge) groups
          print $ sum repeatPriorities
          putStrLn "trolled"
