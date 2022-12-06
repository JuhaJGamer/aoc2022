module Day5 where

import           Control.Applicative
import qualified Data.Array    as Array
import           Data.Array      (Array, array, (//), (!))
import           Data.List.Split
import           Data.Maybe

import           System.IO

newtype Crate  = Crate Char
    deriving (Show, Eq)
data Operation = Move Int Int Int
    deriving (Show, Eq)

type Procedure = [Operation]
type Stack     = [Crate]

transpose = getZipList . traverse ZipList

readCrate :: String -> Maybe Crate
readCrate ['[',c,']'] = Just $ Crate c
readCrate _           = Nothing

-- Reads *padded* (all same length) lines into stacks of crates
readStacks :: [String] -> Array Int Stack 
readStacks = toArray . map catMaybes . transpose . map readCrateLine
    where readCrateLine = map (readCrate . take 3) . chunksOf 4
          toArray xs = array (1, length xs) $ zip [1..] xs

padr :: Int -> Char -> String -> String
padr n c s
  | length s < n = s ++ replicate (n - length s) c
  | otherwise    = s

padLines :: [String] -> [String]
padLines ss = map (padr len ' ') ss
    where len = maximum $ map length ss

readOperation :: String -> Operation
readOperation s
  | head (words s) == "move" = 
      Move (read (words s!!1)) (read (words s!!3)) (read (words s!!5))
  | otherwise = undefined

performOp :: Operation -> Array Int Stack -> Array Int Stack
performOp (Move 0 _ _) stacks = stacks
performOp (Move n a b) stacks = performOp (Move (n-1) a b) newArr
    where crate  = head (stacks!a)
          stack  = tail (stacks!a)
          newArr = stacks // [(a,stack), (b, crate:(stacks!b))]

main = do input <- lines <$> getContents
          let (crateLines:procLines:_) = splitOn [""] input
              crates = readStacks $ padLines crateLines
              proc   = map readOperation procLines
              finalCrates = foldr performOp crates proc
          print $ map ((\(Crate c) -> c) . head) $ Array.elems finalCrates
          putStrLn "trolled"
