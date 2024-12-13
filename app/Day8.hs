{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Day8 where

import Data.Function (on)
import Data.List (groupBy, intercalate, sortBy)
import Data.List.Split (chunksOf)
import qualified Data.Map as M
import Utils (fold_input)

part_1 = do
  terrain <- read_input
  g <- pure . groups $ towers_list terrain
  let pairs = concatMap combinations g
      final_terrain = foldl add_antinode terrain pairs
  pure $ M.size $ M.filterWithKey (\_ v -> v == '#') final_terrain

part_2 = do
  terrain <- read_input
  g <- pure . groups $ towers_list terrain
  let pairs = concatMap combinations g
      final_terrain = foldl add_antinode_2 terrain pairs
  pure $ M.size $ M.filterWithKey (\_ v -> v == '#') final_terrain

print_terrain terrain = do
  let tsize = fst . fst $ M.findMax terrain
  putStrLn $ intercalate "\n" $ map (map snd) $ chunksOf (tsize + 1) $ sortBy (compare `on` fst) $ M.toList terrain

read_input = do
  input <- readFile "app/day8input.txt"
  pure $ fold_input M.empty f input
  where
    f x y = M.insert (x, y)

towers_list = M.filterWithKey (\_ v -> v /= '.')

groups t = filter (\l -> length l > 1) $ map (map fst) $ groupBy ((==) `on` snd) $ sortBy (compare `on` snd) $ M.toList t

combinations [] = []
combinations (h : rest) = [(h, y) | y <- rest] ++ combinations rest

add_antinode terrain (node1, node2) =
  let (xdiff1, xdiff2) = (fst node1 - fst node2, fst node2 - fst node1)
      (ydiff1, ydiff2) = (snd node1 - snd node2, snd node2 - snd node1)
      an1 = (fst node2 - xdiff1, snd node2 - ydiff1)
      an2 = (fst node1 - xdiff2, snd node1 - ydiff2)
   in M.update (\_ -> Just '#') an2 (M.update (\_ -> Just '#') an1 terrain)

add_antinode_2 terrain (node1, node2) =
  let terrain_max = fst . fst $ M.findMax terrain
      xdiff = fst node1 - fst node2
      ydiff = snd node1 - snd node2
      r = [(-(1 * terrain_max)) .. terrain_max]
      values = filter (\(a, b) -> a >= 0 && b >= 0 && a <= terrain_max && b <= terrain_max) $ map (\v -> (fst node1 + v * xdiff, snd node1 + v * ydiff)) r
      f t pair = M.update (\_ -> Just '#') pair t
   in foldl f terrain values