{-# LANGUAGE TupleSections #-}
module Day8 where

import qualified Data.Array as Array
import           Data.Array (Array, array, (!))

import Data.Bifunctor
import Data.Bool
import Data.List
import Data.Either
import Control.Monad

import System.IO

import Debug.Trace

type Tree   = Int
type Coord  = (Int, Int)
type Forest = Array Coord Tree

readLine :: String -> [(Coord, Tree)]
readLine = zipWith (curry (bimap (0,) (read . (:[])))) [0..]

readForest :: [String] -> Forest
readForest xs =
  let assocs = join $ zipWith (map . first . first . const) [0..] $ map readLine xs
      coords = map fst assocs
      bounds = (minimum coords, maximum coords)
   in array bounds assocs

both f = bimap f f

viewDistance :: Forest -> Coord -> Coord -> Either Int Int
viewDistance forest delta coord = foldl viewUntil (Left 0) $ along delta coord
    where along delta coord = tail $ takeWhile inBounds $ scanl coordSum coord $ repeat delta
          stillVisible = (<tree) . (forest!)
          nextVisibility :: Tree -> Coord -> Either Int Int
          nextVisibility old coord
            = if stillVisible coord
                 then Left (old+1)
                 else Right (old+1)
          viewUntil :: Either Int Int -> Coord -> Either Int Int
          viewUntil = either nextVisibility (const . Right)
          inBounds (x,y)
            | x < x0 || x > xmax = False
            | y < y0 || y > ymax = False
            | otherwise = True
                where ((x0,y0), (xmax,ymax)) = Array.bounds forest
          coordSum x = uncurry bimap (both (+) x)
          tree = forest!coord

visibleAlong :: Forest -> Coord -> Coord -> Bool
visibleAlong = ((isLeft .) .) . viewDistance

scenicScore :: Forest -> Coord -> Int
scenicScore forest coord = product $ map (either id id . flip (viewDistance forest) coord) dirDeltas

visible forest coord = any (flip (visibleAlong forest) coord) dirDeltas
dirDeltas = [ ( 0, 1)
            , ( 0,-1)
            , ( 1, 0)
            , (-1, 0)
            ]

main = do forest <- readForest . lines <$> getContents
          print $ length $ filter id $ map (visible forest) $ Array.indices forest
          print $ maximum $ map (scenicScore forest) $ Array.indices forest
          putStrLn "trolled"
