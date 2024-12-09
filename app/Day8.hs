module Day8 where

import Data.Foldable (traverse_)
import Data.Function (on)
import Data.List (groupBy, intercalate, nub, sortBy)
import Data.List.Split (chunksOf)
import qualified Data.Map as M
import Debug.Trace (trace)
import Utils (foldInput)

part1 = do
  terrain <- readInput
  g <- pure . groups $ towerslist terrain
  let pairs = concatMap combinations g
      finalterrain = foldl addantinode terrain pairs
  print $ M.size $ M.filterWithKey (\_ v -> v == '#') finalterrain

part2 = do
  terrain <- readInput
  g <- pure . groups $ towerslist terrain
  let pairs = concatMap combinations g
      finalterrain = foldl addantinode2 terrain pairs
  print $ M.size $ M.filterWithKey (\_ v -> v == '#') finalterrain

-- print $ M.size $ M.filterWithKey (\_ v -> v == '#') $ foldl addantinode2 terrain pairs

printterrain terrain = do
  let tsize = fst . fst $ M.findMax terrain
  putStrLn $ intercalate "\n" $ map (map snd) $ chunksOf (tsize + 1) $ sortBy (compare `on` fst) $ M.toList terrain

readInput = do
  input <- readFile "app/day8input.txt"
  pure $ foldInput M.empty f input
  where
    f x y = M.insert (x, y)

towerslist = M.filterWithKey (\_ v -> v /= '.')

groups t = filter (\l -> length l > 1) $ map (map fst) $ groupBy ((==) `on` snd) $ sortBy (compare `on` snd) $ M.toList t

combinations [] = []
combinations (h : rest) = [(h, y) | y <- rest] ++ combinations rest

addantinode terrain (node1, node2) =
  let (xdiff1, xdiff2) = (fst node1 - fst node2, fst node2 - fst node1)
      (ydiff1, ydiff2) = (snd node1 - snd node2, snd node2 - snd node1)
      an1 = (fst node2 - xdiff1, snd node2 - ydiff1)
      an2 = (fst node1 - xdiff2, snd node1 - ydiff2)
   in M.update (\_ -> Just '#') an2 (M.update (\_ -> Just '#') an1 terrain)

addantinode2 terrain (node1, node2) =
  let terrainMax = fst . fst $ M.findMax terrain
      xdiff = fst node1 - fst node2
      ydiff = snd node1 - snd node2
      r = [(-(1 * terrainMax)) .. terrainMax]
      values = filter (\(a, b) -> a >= 0 && b >= 0 && a <= terrainMax && b <= terrainMax) $ map (\v -> (fst node1 + v * xdiff, snd node1 + v * ydiff)) r
      f t pair = M.update (\_ -> Just '#') pair t
   in foldl f terrain values