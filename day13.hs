module Day13 where

import Data.Bifunctor
import Data.Char
import Data.List.Split

import System.IO

import Debug.Trace

data Tree a = Leaf a | Node [Tree a]
    deriving (Show, Eq)

instance (Show a, Ord a) => Ord (Tree a) where
    a@(Leaf _) <= b@(Node _) = Node [a] <= b
    a@(Node _) <= b@(Leaf _) = a        <= Node [b]
    (Leaf a)   <= (Leaf b) = trace (show (a,b)) (a <= b)
    (Node xs)  <= (Node ys)
       | null xs   = True
       | null ys   = False
       | otherwise = xs <= ys

readTree:: (Read a) => String -> Tree a
readTree xs = let (Node ts, zs) = treenode (xs ++ "]") [] in head ts

treenode :: (Read a)
         => String
         -> [Tree a]
         -> (Tree a, String)
treenode [] _ = error "We were out of tree before we meant to be"
treenode ('[':xs) ts =
    let (n, xs') = treenode xs []
     in treenode xs' (n : ts)
treenode (']':xs) ts = (Node (reverse ts), xs)
treenode (',':xs) ts = treenode xs ts
treenode (x:xs) ts   =
    let numS = takeWhile isDigit (x:xs)
        xs'  = dropWhile isDigit xs
     in treenode xs' (Leaf (read numS) : ts)

main = do
    input <- splitOn [""] . lines <$> getContents
    let pairs :: [(Tree Int, Tree Int)]
        pairs = map (\[a,b] -> (readTree a,readTree b)) input
        ordered = map snd . filter (uncurry (<) . fst) $ zip pairs [1..]
    print $ sum ordered
    putStrLn "trolled"
