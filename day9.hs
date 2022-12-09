module Day9 where

import Data.Bifunctor
import Data.Ord
import Data.List

import System.IO

type Coord = (Int, Int)
data Dir = U | D | L | R
    deriving (Show, Eq)
data Move = Move Dir Int
    deriving (Show, Eq)

both f = bimap f f
coordSum x = uncurry bimap (both (+) x)

chebyshev :: Coord -> Coord -> Int
chebyshev (ax,ay) (bx,by) = max (abs (ay-by)) (abs (ax-bx))

allMoves  = [ (x,y) | x <- [-1..1], y <- [-1..1]]
gridMoves = [ (x,y) | (x,y) <- allMoves, x == 0 || y == 0 ]
diagMoves = [ m | m <- allMoves, m `notElem` gridMoves ]

tailMove :: Coord -> Coord -> Coord
tailMove head@(hx,hy) tail@(tx,ty)
  | chebyshev head tail < 2 = (0,0)
  | hx == tx || hy == ty    = minimumBy (comparing nextChebyshev) gridMoves
  | otherwise               = minimumBy (comparing nextChebyshev) diagMoves
    where nextChebyshev = chebyshev head . coordSum tail

nextTail :: Coord -> Coord -> Coord
nextTail head tail = coordSum tail $ tailMove head tail

nextHead :: Move -> Coord -> Coord
nextHead (Move _ 0) = id
nextHead (Move U _) = coordSum (-1,0)
nextHead (Move D _) = coordSum (1,0)
nextHead (Move L _) = coordSum (0,-1)
nextHead (Move R _) = coordSum (0,1)

nextMoves :: [Move] -> [Move]
nextMoves ((Move _ 0):xs) = xs
nextMoves ((Move d n):xs) = Move d (n-1) : xs

movePath :: [Move] -> Coord -> Coord -> [(Coord, Coord)]
movePath [] head tail = [(head, tail)]
movePath ms@(m:_) head tail =
  let head' = nextHead m head
      tail' = nextTail head' tail
      ms'   = nextMoves ms
   in (head, tail):movePath ms' head' tail'

readMove :: String -> Move
readMove (d:' ':s)
  = flip Move (read s) $
      case d of
        'U' -> U
        'D' -> D
        'L' -> L
        'R' -> R

main = do moves <- map readMove . lines <$> getContents
          let path          = movePath moves (0,0) (0,0)
              tailPositions = nub $ sort $ map snd path
          print $ length tailPositions
          putStrLn "trolled"
