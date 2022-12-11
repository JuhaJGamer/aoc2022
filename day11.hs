module Day11 where

import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map, (!))

import Data.Bifunctor
import Data.Bool
import Data.Char
import Data.List
import Data.List.Split
import Data.Ord

import Debug.Trace

import System.IO

type MonkeyId = Int
type WorryLvl = Int
data Test = Test { getMod :: Int
                 , getT :: MonkeyId
                 , getF :: MonkeyId
                 }
    deriving (Show, Eq)
data Monkey = Monkey { getOp :: WorryLvl -> WorryLvl
                     , getTest :: Test
                     }
type Monkeys = [(MonkeyId,Monkey)]
type Items   = Map MonkeyId [WorryLvl]
type Round   = (Items, Map MonkeyId Int)

type MonkeyProcess = (Monkey -> WorryLvl -> (MonkeyId, WorryLvl))
type MonkeyProcess' = WorryLvl -> MonkeyProcess

instance Show Monkey where
    show m = "Monkey " ++ show (getOp m 1) ++ " " ++ show (getTest m)

combineRounds :: Round -> Round -> Round
combineRounds (_, nMap) (iMap, nMap') = (iMap, nMap'')
    where nMap'' = foldr (uncurry $ Map.insertWith (+)) nMap
                 $ Map.assocs nMap'

processItem :: WorryLvl -> MonkeyProcess
processItem maxWorry (Monkey op (Test m t f)) worry = (test newWorry, newWorry)
    where newWorry = floor (fromIntegral (op worry) / 3) `mod` maxWorry
          test     = bool t f . (/= 0) . (`mod` m)

processItem2 :: WorryLvl -> MonkeyProcess
processItem2 maxWorry (Monkey op (Test m t f)) worry = (test newWorry, newWorry)
    where newWorry = floor (fromIntegral (op worry)) `mod` maxWorry
          test     = bool t f . (/= 0) . (`mod` m)

monkeyRound :: MonkeyProcess -> Monkey -> [WorryLvl] -> [(MonkeyId, [WorryLvl])]
monkeyRound process monkey worry
  = collect $ map (process monkey) worry
    where collect = foldr collect' [] . sortBy (comparing fst)
          collect' (i,x) [] = [(i,[x])]
          collect' (i,x) ((j,xs):is)
            | i == j = (i,x:xs):is
            | i /= j = (i,[x]):(j,xs):is

fullRound :: MonkeyProcess' -> Monkeys -> Items -> Round
fullRound proc ms iMap = foldl (flip monkeyRound') (iMap, Map.empty) ms
    where monkeyRound' :: (MonkeyId, Monkey)
                       -> Round
                       -> Round
          monkeyRound' (mId,m) (iMap, nMap)
            = let is'    = monkeyRound (proc maxWorry) m (iMap!mId)
                  iMap'  = foldr (uncurry (Map.insertWith (++))) iMap is'
                  iMap'' = Map.insert mId [] iMap'
                  nMap'  = Map.insert mId (length $ snd =<< is') nMap
               in (iMap'', nMap')
          maxWorry :: Int
          maxWorry = foldr (lcm . getMod . getTest . snd) 1 ms

nRounds :: MonkeyProcess' -> Monkeys -> Items -> Int -> Round
nRounds proc ms iMap n
  = foldl' (\r@(is,_) _ -> combineRounds r $ fullRound proc ms is) emptyRound [1..n]
    where emptyRound = (iMap, Map.empty)

parseMonkey :: [String] -> (MonkeyId, Monkey, [WorryLvl])
parseMonkey (idStr:isStr:opStr:pStrs) = (mId, m, is)
    where mId = read $ takeWhile isNumber $ drop 7 idStr
          is = map read . splitOn ", " $ drop 18 isStr
          op = parseOpExpr $ drop 19 opStr
          parseOpExpr s
            | s == "old * old" = (^2)
            | take 5 s == "old +" = (+(read $ drop 5 s))
            | take 5 s == "old *" = (*(read $ drop 5 s))
            | otherwise = error $ "Unexpected operation '" ++ s ++ "'"
          testMod = read $ drop 21 (head pStrs)
          testOutcomes = (targetMonkey (pStrs!!1), targetMonkey (pStrs!!2))
          targetMonkey = read . (!!5) . splitOn " " . dropWhile isSpace
          test = uncurry (Test testMod) testOutcomes
          m = Monkey op test

parseMonkeys :: [String] -> (Monkeys, Items)
parseMonkeys = first (sortOn fst)
             . foldl' (flip collect) ([], Map.empty)
             . map parseMonkey
             . splitOn [""]
    where collect (mId, m, is) (ms, iMap) = ((mId,m):ms,Map.insert mId is iMap)
          collect :: (MonkeyId, Monkey, [WorryLvl])
                  -> (Monkeys, Items)
                  -> (Monkeys, Items)

monkeyBusiness :: Round -> Int 
monkeyBusiness = uncurry (*) . topTwo
    where topTwo = (\[a,b] -> (a,b))
                 . take 2
                 . reverse
                 . sort
                 . map snd
                 . Map.assocs
                 . snd 

main = do
    (ms, is) <- parseMonkeys . lines <$> getContents
    let rounds1 = nRounds processItem  ms is 20
        rounds2 = nRounds processItem2 ms is 10000 
    print $ monkeyBusiness rounds1
    print $ monkeyBusiness rounds2
    putStrLn "trolled"
