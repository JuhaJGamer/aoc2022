{-# LANGUAGE TupleSections #-}

module Day12 where

import qualified Data.Array as Array
import           Data.Array (Array, array, (!), (//))

import Data.Maybe
import Data.Char
import Data.List

import System.IO

import Debug.Trace

type Coord = (Int,Int)
type Altitude = Int
type Heightmap = Array Coord Altitude

data FIFO a = FIFO [a] [a]

instance (Show a) => Show (FIFO a) where
    show fifo = "FIFO " ++ show (fifoToList fifo)

instance Functor FIFO where
    fmap f (FIFO as bs) = FIFO (f <$> as) (f <$> bs)

instance Applicative FIFO where
    fq <*> bq = fifoFromList (fifoToList fq <*> fifoToList bq)
    pure x = FIFO [] [x]

enqueue :: a -> FIFO a -> FIFO a
enqueue x (FIFO as bs) = FIFO (x:as) bs

dequeue :: FIFO a -> Maybe (a, FIFO a)
dequeue (FIFO as bs)
  | null as && null bs = Nothing
  | null bs            = dequeue $ FIFO [] (reverse as)
  | otherwise          = Just (head bs, FIFO as $ tail bs)

fifoFromList :: [a] -> FIFO a
fifoFromList = foldl (flip enqueue) (FIFO [] [])

fifoToList (FIFO as bs) = bs ++ reverse as

(^+^) :: Coord -> Coord -> Coord
(ax,ay) ^+^ (bx,by) = (ax+bx, ay+by)

readHeight :: Char -> Altitude
readHeight c
  | 'a' <= c && c <= 'z' = ord c   - ord 'a'
  | c == 'E'             = ord 'z' - ord 'a'
  | c == 'S'             = ord 'a' - ord 'a'

findChar :: Char -> [String] -> Coord
findChar c ss = fst . head
              $ filter ((c==) . snd) assocs
    where assocs = do
            (s, y) <- zip ss [0..]
            (c, x) <- zip s [0..]
            return ((y,x),c)

readHeightmap :: [String] -> (Heightmap, Coord, Coord)
readHeightmap ss = (hs, start, end)
    where hAssocs = do
            (s, y) <- zip ss [0..]
            (c, x) <- zip s  [0..]
            return ((y,x), readHeight c)
          bounds = (minimum (map fst hAssocs), maximum (map fst hAssocs))
          hs = array bounds hAssocs
          start = findChar 'S' ss
          end   = findChar 'E' ss

findPath :: Coord -> Coord -> Heightmap -> Maybe [Coord]
findPath start end hMap = bfs (pure start) emptyArr
    where emptyArr = Nothing <$ hMap
          bfs fifo predMat = do
              (cur,fifo') <- dequeue fifo
              let neighbours = filter notVisited
                             . filter allowedStep
                             . filter inBounds
                             . map (cur ^+^)
                             $ [(0,1),(1,0),(0,-1),(-1,0)]
                  allowedStep = (<= 1) . subtract (hMap!cur) . (hMap !)
                  notVisited  = isNothing . (predMat !)
                  inBounds (y,x)
                     | x0 > x || x > xMax = False
                     | y0 > y || y > yMax = False
                     | otherwise          = True
                     where ((y0,x0),(yMax,xMax)) = Array.bounds hMap
                  fifo'' = foldl (flip enqueue) fifo' neighbours
                  predMat' = predMat // map (,Just cur) neighbours
              if cur == end
                  then Just $ backtrack (Just cur) predMat
                  else bfs fifo'' predMat'
          backtrack (Just c) predMat
            | c == start = [c]
            | otherwise  = c : backtrack (predMat!c) predMat

main = do
    (hs, start, end) <- readHeightmap . lines <$> getContents
    let lowPoints = map fst
                  . filter ((== readHeight 'a') . snd)
                  $ Array.assocs hs
        paths     = mapMaybe (flip (`findPath` end) hs) lowPoints
    case findPath start end hs of
        Nothing -> putStrLn "No path from start to end!"
        Just p  -> print . subtract 1 . length $ p
    print . subtract 1 . minimum . map length $ paths
    putStrLn "trolled"
