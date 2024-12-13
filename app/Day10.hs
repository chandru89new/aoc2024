{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

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
part1 = do
  terrain <- readinput
  pure $ length $ nub $ validpaths terrain (trailheads' terrain) []

part2 :: IO Int
part2 = do
  terrain <- readinput
  pure $ length $ validpaths terrain (trailheads' terrain) []

readinput = do
  input <- readFile "app/day10input.txt"
  pure $ foldinput M.empty f input
  where
    f x y c = M.insert (x, y) (read [c] :: Int)

-- types
type Coord = (Int, Int)

type Elevation = Int

type Terrain = M.Map Coord Elevation

type A = (Int, Int, Int)

getneighbors :: Int -> Coord -> [Coord]
getneighbors max (x, y) = [(x, y) | (x, y) <- [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)], x >= 0 && x <= max && y >= 0 && y <= max]

getmax :: Terrain -> Int
getmax terrain = fst . fst $ M.findMax terrain

formatterrain :: Terrain -> [A]
formatterrain terrain = sort $ map f $ M.toList terrain
  where
    f (c, e) = (e, fst c, snd c)

validpaths :: Terrain -> [(A, A)] -> [(A, A)] -> [(A, A)]
validpaths ter [] res = res
validpaths ter (h : rest) acc =
  let ((id, x2, y2), (s, x1, y1)) = h
      newlist = neighbors ++ rest
      neighbors = map (\i -> (i, snd h)) $ foldl f [] $ map (\k -> (k, M.lookup k ter)) $ getneighbors (getmax ter) (x2, y2)
      f b (_, Nothing) = b
      f b (k, Just v) = if v == id + 1 then (v, fst k, snd k) : b else b
   in if id == 9
        then validpaths ter rest (acc ++ [h])
        else validpaths ter newlist acc

trailheads :: Terrain -> [A]
trailheads ter = map (\(k, v) -> (v, fst k, snd k)) $ M.toList $ M.filterWithKey (\k v -> v == 0) ter

trailheads' ter = map (\c -> (c, c)) $ trailheads ter

-- test

terrain' :: M.Map (Int, Int) Int
terrain' = M.fromList [((0, 0), 8), ((0, 1), 9), ((0, 2), 0), ((0, 3), 1), ((0, 4), 0), ((0, 5), 1), ((0, 6), 2), ((0, 7), 3), ((1, 0), 7), ((1, 1), 8), ((1, 2), 1), ((1, 3), 2), ((1, 4), 1), ((1, 5), 8), ((1, 6), 7), ((1, 7), 4), ((2, 0), 8), ((2, 1), 7), ((2, 2), 4), ((2, 3), 3), ((2, 4), 0), ((2, 5), 9), ((2, 6), 6), ((2, 7), 5), ((3, 0), 9), ((3, 1), 6), ((3, 2), 5), ((3, 3), 4), ((3, 4), 9), ((3, 5), 8), ((3, 6), 7), ((3, 7), 4), ((4, 0), 4), ((4, 1), 5), ((4, 2), 6), ((4, 3), 7), ((4, 4), 8), ((4, 5), 9), ((4, 6), 0), ((4, 7), 3), ((5, 0), 3), ((5, 1), 2), ((5, 2), 0), ((5, 3), 1), ((5, 4), 9), ((5, 5), 0), ((5, 6), 1), ((5, 7), 2), ((6, 0), 0), ((6, 1), 1), ((6, 2), 3), ((6, 3), 2), ((6, 4), 9), ((6, 5), 8), ((6, 6), 0), ((6, 7), 1), ((7, 0), 1), ((7, 1), 0), ((7, 2), 4), ((7, 3), 5), ((7, 4), 6), ((7, 5), 7), ((7, 6), 3), ((7, 7), 2)]