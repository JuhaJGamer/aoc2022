module Day14 where

import qualified Data.Array as Array
import           Data.Array (Array, array, (!), (//))

import Control.Monad
import Data.List
import Data.List.Split
import Data.Bifunctor

import System.IO

data Tile = Air | Rock | Sand
    deriving (Eq, Show)
type Coord = (Int,Int)
type CaveScan = Array Coord Tile

(^+^) :: Coord -> Coord -> Coord

(^+^) x = uncurry bimap (both (+) x)

both f = bimap f f

isSand Sand = True
isSand _    = False

parseStructure :: String -> [Coord]
parseStructure = map (both read . tuplify . splitOn ",")
               . splitOn " -> "
    where tuplify [a,b] = (b, a)

createScan :: [[Coord]] -> CaveScan
createScan xs = foldl' addStructure emptyScan xs
    where emptyScan = Array.listArray bounds $ repeat Air
          bounds =
              let xs' = (0,500): join xs
                  mins = (minimum $ map fst xs', minimum $ map snd xs')
                  maxs = (maximum $ map fst xs', maximum $ map snd xs')
               in (mins, maxs)
          addStructure scan []  = scan
          addStructure scan [_] = scan
          addStructure scan ((ay,ax):(by,bx):xs)
            | ax == bx =
                let start = min ay by
                    end   = max ay by
                    scan' = scan // [((y,ax),Rock) | y <- [start..end]]
                 in addStructure scan' ((by,bx):xs)
            | ay == by =
                let start = min ax bx
                    end   = max ax bx
                    scan' = scan // [((ay,x),Rock) | x <- [start..end]]
                 in addStructure scan' ((by,bx):xs)
            | otherwise = error "Invalid segment"

dropSand :: Coord -> CaveScan -> Maybe Coord
dropSand sand scan
  | not (inBounds sand) = Nothing
  | canFall             =  dropSand (head next) scan
  | otherwise           = Just sand
    where inBounds (y,x) =
            let ((y0,x0), (ymax,xmax)) = Array.bounds scan
             in and [x0 <= x, x <= xmax, y0 <= y, y <= ymax]
          canFall = not $ null next
          candidates = map (sand ^+^) [(1,0), (1,-1), (1,1)]
          next = filter (\c -> not (inBounds c) || noObstacle c) candidates
          noObstacle c =
            case scan!c of
                Air -> True
                _   -> False

dropUntilAbyss :: Coord -> CaveScan -> CaveScan
dropUntilAbyss source scan = 
   case dropSand source scan of 
     Nothing -> scan
     Just sand -> dropUntilAbyss source (scan // [(sand, Sand)])

main = do
    input <- map parseStructure . lines <$> getContents
    let scan  = createScan input
        scan' = dropUntilAbyss (0,500) scan
    print $ length $ filter (isSand . snd) $ Array.assocs scan'
    putStrLn "trolled"
