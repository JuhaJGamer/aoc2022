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
data Monkey = Monkey { getOp :: WorryLvl -> WorryLvl
                     , getTest :: WorryLvl -> MonkeyId
                     }
type Monkeys = [(MonkeyId,Monkey)]
type Items   = Map MonkeyId [WorryLvl]
type Round   = (Items, Map MonkeyId Int)

instance Show Monkey where
    show m = "Monkey " ++ show (getOp m 1)

combineRounds :: Round -> Round -> Round
combineRounds (_, nMap) (iMap, nMap') = (iMap, nMap'')
    where nMap'' = foldr (uncurry $ Map.insertWith (+)) nMap
                 $ Map.assocs nMap'

processItem :: Monkey -> WorryLvl -> (MonkeyId, WorryLvl)
processItem (Monkey op test) worry = (test newWorry, newWorry)
    where newWorry = floor (fromIntegral (op worry) / 3)

monkeyRound :: Monkey -> [WorryLvl] -> [(MonkeyId, [WorryLvl])]
monkeyRound monkey worry = collect $ map (processItem monkey) worry
    where collect = foldr collect' [] . sortBy (comparing fst)
          collect' (i,x) [] = [(i,[x])]
          collect' (i,x) ((j,xs):is)
            | i == j = (i,x:xs):is
            | i /= j = (i,[x]):(j,xs):is

fullRound :: Monkeys -> Items -> Round
fullRound ms iMap = foldl (flip monkeyRound') (iMap, Map.empty) ms
    where monkeyRound' :: (MonkeyId, Monkey)
                       -> Round
                       -> Round
          monkeyRound' (mId,m) (iMap, nMap)
            = let is'    = monkeyRound m (iMap!mId)
                  iMap'  = foldr (uncurry (Map.insertWith (++))) iMap is'
                  iMap'' = Map.insert mId [] iMap'
                  nMap'  = Map.insert mId (length $ snd =<< is') nMap
               in (iMap'', nMap')

nRounds :: Monkeys -> Items -> Int -> Round
nRounds ms iMap n
  = foldl' (\r@(is,_) _ -> combineRounds r $ fullRound ms is) emptyRound [1..n]
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
          testPred = (/= 0) . (`mod` (read $ drop 21 (head pStrs)))
          testOutcomes = (targetMonkey (pStrs!!1), targetMonkey (pStrs!!2))
          targetMonkey = read . (!!5) . splitOn " " . dropWhile isSpace
          test = uncurry bool testOutcomes . testPred
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


main = do
    (ms, is) <- parseMonkeys . lines <$> getContents
    let (_, inspections) =  nRounds ms is 20
        topTwo = (\[a,b] -> (a,b))
               . take 2 
               . reverse
               . sort 
               . map snd 
               $ Map.assocs inspections
        monkeyBusiness = uncurry (*) topTwo
    print monkeyBusiness
    putStrLn "trolled"
