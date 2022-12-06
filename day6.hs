module Day6 where

import Data.List.Split
import Data.List

newtype Packet = Packet String
    deriving (Show, Eq)

packets :: Int -> String -> [Packet]
packets n = map (Packet . (fst <$>)) . splitWhen (delimiter . snd) . tailPairs
    where delimiter xs = nub (take n xs) == take n xs && length (take n xs) == n
          tailPairs xs = zip xs (tails xs)

startOffset :: Int -> String -> Int
startOffset n s = n + length ((\(Packet s) -> s) $ head $ packets n s)

main = do stream <- head . lines <$> getContents
          print $ startOffset 4 stream
          print $ startOffset 14 stream
          putStrLn "trolled"

