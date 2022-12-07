module Day7 where

import qualified Data.Map as Map
import           Data.Map (Map, (!))

import Data.Bifunctor
import Data.List
import Data.Ord

data FSObject = Dir (Map String FSObject) | File Int
    deriving (Show, Eq)

readFSObject :: String -> (String, FSObject)
readFSObject s
  | take 3 s == "dir" = (drop 4 s, Dir Map.empty)
  | otherwise =
      let (size, name) = break (== ' ') s
       in (drop 1 name, File $ read size)

constructFSTree :: [String] -> FSObject
constructFSTree = snd . foldl consumeLine ([], Dir Map.empty)
    where consumeLine state@(wds, root) s
            | take 4 s == "$ cd" = changePwd state $ drop 4 s
            | take 4 s == "$ ls" = state
            | otherwise = (wds, insertInto root (reverse wds) $ readFSObject s)
          changePwd (wds, root) s
            | s == " /"  = ([], root)
            | s == " .." = (tail wds, root)
            | otherwise  = (drop 1 s:wds, root)
          insertInto (Dir filemap) (wd:wds) fsobj
            = Dir $ Map.insert wd (insertInto (filemap!wd) wds fsobj) filemap
          insertInto (Dir filemap) [] (name, fsobj)
            = Dir $ Map.insert name fsobj filemap

size :: FSObject -> Int
size (File x) = x
size (Dir filemap) = foldr ((+) . size) 0 filemap

flat :: FSObject -> [(String, FSObject)]
flat (File _) = []
flat (Dir filemap)
  = Map.assocs filemap ++ (Map.assocs filemap >>= (flat . snd))

dirs :: [(String, FSObject)] -> [(String, FSObject)]
dirs = filter (isDir . snd)
    where isDir (File _) = False
          isDir _        = True

main = do input <- lines <$> getContents
          let fstree = constructFSTree input
              dirsizes = map (second size) $ dirs $ flat fstree
              smalldirs = filter ((<=100000) . snd) dirsizes
              unused = 70000000 - size fstree
              reqsize = 30000000 - unused
              bigdirs = filter ((>=reqsize) . snd) dirsizes
          print $ sum $ map snd smalldirs
          print $ minimumBy (comparing snd) bigdirs 
          putStrLn "trolled"
