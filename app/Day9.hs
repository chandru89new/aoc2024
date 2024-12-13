{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Day9 where

import Control.Applicative (Applicative (liftA2))
import Control.Monad (join)
import Data.Char (isDigit)
import Data.List (elemIndex, find, findIndex, intercalate, maximumBy, nub)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import Utils (foldinput)

part_1 :: IO Int
part_1 = do
  input <- read_input
  let asblocks = convert_into_blocks input
  pure $ get_checksum $ defrag asblocks

part_2 :: IO Int
part_2 = do
  input <- read_input
  let asblocks = format_input input
  pure $ get_checksum' $ show_defrag $ defrag' asblocks (max_fid asblocks)

read_input = readFile "app/day9input.txt"

convert_into_blocks = go 0 []
  where
    go :: Int -> [String] -> String -> [String]
    go _ acc [] = acc
    go n acc (h : rest)
      | even n = go (n + 1) (acc ++ replicate (read [h] :: Int) (show (n `div` 2))) rest
      | otherwise = go (n + 1) (acc ++ replicate (read [h] :: Int) ".") rest

idx_of_first_dot = elemIndex "."

update_at_idx idx nv list = take idx list ++ [nv] ++ drop (idx + 1) list

defrag_once list =
  let hasnogaps = no_gaps list
      firstdotidx = idx_of_first_dot list
      (lastdigit, newlist) = pop_last_digit list
   in if hasnogaps
        then list
        else case (firstdotidx, lastdigit) of
          (Just idx, Just n) -> update_at_idx idx n newlist
          _ -> newlist

no_gaps l = ["."] == nub (dropWhile (/= ".") l)

pop_last_digit l = go (length l - 1) (reverse l)
  where
    go idx [] = (Nothing, [])
    go idx (h : rest) = if h == "." then go (idx - 1) rest else (Just h, update_at_idx idx "." l)

defrag l = if no_gaps l then l else defrag (defrag_once l)

get_checksum = go 0 0
  where
    go idx acc [] = acc
    go idx acc (h : rest) = if h == "." then acc else go (idx + 1) (acc + idx * read h) rest

get_checksum' = go 0 0
  where
    go idx acc [] = acc
    go idx acc (h : rest) = if h == "." then go (idx + 1) acc rest else go (idx + 1) (acc + idx * read h) rest

data B = Empty | Fid Int deriving (Eq, Ord, Show)

format_input str = reverse (filter (\(b, v) -> not (b == Empty && v == 0)) (go 0 [] str))
  where
    go _ acc [] = acc
    go idx acc (h : rest)
      | even idx = go (idx + 1) ((Fid $ div idx 2, read [h] :: Int) : acc) rest
      | otherwise = go (idx + 1) ((Empty, read [h]) : acc) rest

find_empty_slot_with_idx nspace list = result
  where
    result = liftA2 (,) a b
    a = find f list
    b = findIndex f list
    f (a, n) = case a of
      Fid _ -> False
      Empty -> n >= nspace

insert_at_idx idx val list = take idx list ++ [val] ++ drop idx list

move_blocks (fileblock, idx1) (emptyblock, idx') list =
  insert_at_idx idx' fileblock $ update_at_idx idx' (fst emptyblock, snd emptyblock - snd fileblock) $ update_at_idx idx1 (Empty, snd fileblock) list

defrag_once' fid list = remove0empty updatedlist
  where
    updatedlist = case (fwithidx, ebwithidx) of
      (Just fwi, Just ebwi) -> move_blocks fwi ebwi list
      _ -> list
    f = find (\(b, _) -> b == Fid fid) list
    i = findIndex (\b -> Just b == f) list
    fwithidx = case (f, i) of
      (Just f', Just i') -> Just (f', i')
      _ -> Nothing
    ebwithidx = case fwithidx of
      Just (f', i') -> find_empty_slot_with_idx (snd f') (take i' list)
      _ -> Nothing

remove0empty = filter (\b -> not (fst b == Empty && snd b == 0))

defrag' list startMax
  | startMax < 0 = list
  | otherwise = defrag' (defrag_once' startMax list) (startMax - 1)

show_defrag = go []
  where
    go acc [] = acc
    go acc (h : rest) = go (acc ++ show' h) rest
    show' (b, n) = replicate n (showB b)
    showB (Fid id) = show id
    showB Empty = "."

max_fid :: [(B, Int)] -> Int
max_fid list = case maximumBy (\a b -> compare (fst a) (fst b)) list of
  (Fid id, _) -> id
  _ -> -1