module Day13 where

import Data.Bifunctor
import Data.Char
import Data.List
import Data.List.Split

import System.IO

import Debug.Trace

data Tree a = Leaf a | Node [Tree a]

instance (Show a) => Show (Tree a) where
    show (Node xs) = show xs
    show (Leaf a)  = show a

instance (Eq a) => Eq (Tree a) where
    (Leaf a)   == (Leaf b) = a == b
    a@(Leaf _) == b@(Node _) = Node [a] ==  b
    a@(Node _) == b@(Leaf _) = a == Node [b]
    (Node []) == (Node []) = True
    (Node xs) == (Node ys) = length xs == length ys && all (uncurry (==)) (zip xs ys)
    
    
instance (Show a, Ord a) => Ord (Tree a) where
    compare a@(Leaf _) b@(Node _) = compare (Node [a]) b
    compare a@(Node _) b@(Leaf _) = compare a $ Node [b]
    compare (Leaf a) (Leaf b) = compare a b
    compare (Node []) (Node []) = EQ
    compare (Node []) (Node _)  = LT
    compare (Node _ ) (Node []) = GT
    compare (Node (x:xs)) (Node (y:ys))
       | x < y  = LT
       | x == y = compare (Node xs) (Node ys) 
       | x >  y = GT 

    a <= b = a<b

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
    input <- map readTree . filter (not . null) . lines <$> getContents
    let pairs :: [(Tree Int, Tree Int)]
        pairs = map (\[a,b] -> (a,b)) $ chunksOf 2 input
        preOrdered = map snd . filter (uncurry (<) . fst) $ zip pairs [1..]
        dividers = [readTree "[[2]]", readTree "[[6]]"]
        ordered = sort (dividers ++ input)
        divIndices = map snd . filter ((`elem` dividers) . fst) $ zip ordered [1..] 
    print $ sum preOrdered
    print $ product divIndices
    putStrLn "trolled"
