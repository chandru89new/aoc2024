module Day9 where

import Control.Applicative (Applicative (liftA2))
import Control.Monad (join)
import Data.Char (isDigit)
import Data.List (elemIndex, find, findIndex, intercalate, maximumBy, nub)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import Utils (foldinput)

part1 :: IO Int
part1 = do
  input <- readinput
  let asblocks = convertintoblocks input
  pure $ getchecksum $ defrag asblocks

part2 :: IO Int
part2 = do
  input <- readinput
  let asblocks = formatinput input
  pure $ getchecksum' $ showdefrag $ defrag' asblocks (maxfid asblocks)

readinput = readFile "app/day9input.txt"

convertintoblocks = go 0 []
  where
    go :: Int -> [String] -> String -> [String]
    go _ acc [] = acc
    go n acc (h : rest)
      | even n = go (n + 1) (acc ++ replicate (read [h] :: Int) (show (n `div` 2))) rest
      | otherwise = go (n + 1) (acc ++ replicate (read [h] :: Int) ".") rest

idxoffirstdot = elemIndex "."

updateatidx idx nv list = take idx list ++ [nv] ++ drop (idx + 1) list

defragonce list =
  let hasnogaps = nogaps list
      firstdotidx = idxoffirstdot list
      (lastdigit, newlist) = poplastdigit list
   in if hasnogaps
        then list
        else case (firstdotidx, lastdigit) of
          (Just idx, Just n) -> updateatidx idx n newlist
          _ -> newlist

nogaps l = ["."] == nub (dropWhile (/= ".") l)

poplastdigit l = go (length l - 1) (reverse l)
  where
    go idx [] = (Nothing, [])
    go idx (h : rest) = if h == "." then go (idx - 1) rest else (Just h, updateatidx idx "." l)

defrag l = if nogaps l then l else defrag (defragonce l)

getchecksum = go 0 0
  where
    go idx acc [] = acc
    go idx acc (h : rest) = if h == "." then acc else go (idx + 1) (acc + idx * read h) rest

getchecksum' = go 0 0
  where
    go idx acc [] = acc
    go idx acc (h : rest) = if h == "." then go (idx + 1) acc rest else go (idx + 1) (acc + idx * read h) rest

data B = Empty | Fid Int deriving (Eq, Ord, Show)

formatinput str = reverse (filter (\(b, v) -> not (b == Empty && v == 0)) (go 0 [] str))
  where
    go _ acc [] = acc
    go idx acc (h : rest)
      | even idx = go (idx + 1) ((Fid $ div idx 2, read [h] :: Int) : acc) rest
      | otherwise = go (idx + 1) ((Empty, read [h]) : acc) rest

findemptyslotwithidx nspace list = result
  where
    result = liftA2 (,) a b
    a = find f list
    b = findIndex f list
    f (a, n) = case a of
      Fid _ -> False
      Empty -> n >= nspace

insertatidx idx val list = take idx list ++ [val] ++ drop idx list

moveblocks (fileblock, idx1) (emptyblock, idx') list =
  insertatidx idx' fileblock $ updateatidx idx' (fst emptyblock, snd emptyblock - snd fileblock) $ updateatidx idx1 (Empty, snd fileblock) list

defragonce' fid list = remove0empty updatedlist
  where
    updatedlist = case (fwithidx, ebwithidx) of
      (Just fwi, Just ebwi) -> moveblocks fwi ebwi list
      _ -> list
    f = find (\(b, _) -> b == Fid fid) list
    i = findIndex (\b -> Just b == f) list
    fwithidx = case (f, i) of
      (Just f', Just i') -> Just (f', i')
      _ -> Nothing
    ebwithidx = case fwithidx of
      Just (f', i') -> findemptyslotwithidx (snd f') (take i' list)
      _ -> Nothing

remove0empty = filter (\b -> not (fst b == Empty && snd b == 0))

defrag' list startMax
  | startMax < 0 = list
  | otherwise = defrag' (defragonce' startMax list) (startMax - 1)

showdefrag = go []
  where
    go acc [] = acc
    go acc (h : rest) = go (acc ++ show' h) rest
    show' (b, n) = replicate n (showB b)
    showB (Fid id) = show id
    showB Empty = "."

maxfid :: [(B, Int)] -> Int
maxfid list = case maximumBy (\a b -> compare (fst a) (fst b)) list of
  (Fid id, _) -> id
  _ -> -1