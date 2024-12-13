{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Day10 where

import Data.Bifunctor (Bifunctor (second))
import Data.Function ((&))
import Data.List (nub, sort)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Traversable (for)
import Debug.Trace (trace)
import Utils (foldinput)

-- part1 :: IO Int
part_1 = do
  terrain <- read_input
  pure $ length $ nub $ valid_paths terrain (trail_heads' terrain) []

part_2 :: IO Int
part_2 = do
  terrain <- read_input
  pure $ length $ valid_paths terrain (trail_heads' terrain) []

read_input = do
  input <- readFile "app/day10input.txt"
  pure $ foldinput M.empty f input
  where
    f x y c = M.insert (x, y) (read [c] :: Int)

-- types
type Coord = (Int, Int)

type Elevation = Int

type Terrain = M.Map Coord Elevation

type A = (Int, Int, Int)

get_neighbors :: Int -> Coord -> [Coord]
get_neighbors max (x, y) = [(x, y) | (x, y) <- [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)], x >= 0 && x <= max && y >= 0 && y <= max]

get_max :: Terrain -> Int
get_max terrain = fst . fst $ M.findMax terrain

format_terrain :: Terrain -> [A]
format_terrain terrain = sort $ map f $ M.toList terrain
  where
    f (c, e) = (e, fst c, snd c)

valid_paths :: Terrain -> [(A, A)] -> [(A, A)] -> [(A, A)]
valid_paths ter [] res = res
valid_paths ter (h : rest) acc =
  let ((id, x2, y2), (s, x1, y1)) = h
      newlist = neighbors ++ rest
      neighbors = map (\i -> (i, snd h)) $ foldl f [] $ map (\k -> (k, M.lookup k ter)) $ get_neighbors (get_max ter) (x2, y2)
      f b (_, Nothing) = b
      f b (k, Just v) = if v == id + 1 then (v, fst k, snd k) : b else b
   in if id == 9
        then valid_paths ter rest (acc ++ [h])
        else valid_paths ter newlist acc

trail_heads :: Terrain -> [A]
trail_heads ter = map (\(k, v) -> (v, fst k, snd k)) $ M.toList $ M.filterWithKey (\k v -> v == 0) ter

trail_heads' ter = map (\c -> (c, c)) $ trail_heads ter

-- test

terrain' :: M.Map (Int, Int) Int
terrain' = M.fromList [((0, 0), 8), ((0, 1), 9), ((0, 2), 0), ((0, 3), 1), ((0, 4), 0), ((0, 5), 1), ((0, 6), 2), ((0, 7), 3), ((1, 0), 7), ((1, 1), 8), ((1, 2), 1), ((1, 3), 2), ((1, 4), 1), ((1, 5), 8), ((1, 6), 7), ((1, 7), 4), ((2, 0), 8), ((2, 1), 7), ((2, 2), 4), ((2, 3), 3), ((2, 4), 0), ((2, 5), 9), ((2, 6), 6), ((2, 7), 5), ((3, 0), 9), ((3, 1), 6), ((3, 2), 5), ((3, 3), 4), ((3, 4), 9), ((3, 5), 8), ((3, 6), 7), ((3, 7), 4), ((4, 0), 4), ((4, 1), 5), ((4, 2), 6), ((4, 3), 7), ((4, 4), 8), ((4, 5), 9), ((4, 6), 0), ((4, 7), 3), ((5, 0), 3), ((5, 1), 2), ((5, 2), 0), ((5, 3), 1), ((5, 4), 9), ((5, 5), 0), ((5, 6), 1), ((5, 7), 2), ((6, 0), 0), ((6, 1), 1), ((6, 2), 3), ((6, 3), 2), ((6, 4), 9), ((6, 5), 8), ((6, 6), 0), ((6, 7), 1), ((7, 0), 1), ((7, 1), 0), ((7, 2), 4), ((7, 3), 5), ((7, 4), 6), ((7, 5), 7), ((7, 6), 3), ((7, 7), 2)]