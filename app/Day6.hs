{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Day6 where

import Control.Concurrent.Async (mapConcurrently)
import Data.List (nub)
import qualified Data.Map as M
import Utils (fold_input)

read_input = do
  input <- readFile "app/day6input.txt"
  pure $ fold_input ((0, 0, '>'), M.empty) f input
  where
    f x y c (g, terrain) = (if c `elem` ['^', '>', 'v', '<'] then (x, y, c) else g, M.insert (x, y) c terrain)

part_1 = do
  (sp, ter) <- read_input
  pure $ length $ get_walked_list sp ter []

part_2 = do
  (sp, ter) <- read_input
  let newmaps = map (\w -> M.insert w '#' ter) $ get_walked_list sp ter []
  res <- mapConcurrently (pure . loops sp []) newmaps
  pure $ length $ filter id res

get_walked_list sp@(x, y, dir) ter walked =
  let first_obst = get_next_obstacle ter sp
      newsp = new_starting_point <$> first_obst
      terrain_size = fst . fst $ M.findMax ter
      get_paths (a, b, '^') (a', _) = [(x', b) | x' <- [a' .. a]]
      get_paths (a, b, 'v') (a', _) = [(x', b) | x' <- [a .. a']]
      get_paths (a, b, '>') (_, b') = [(a, y') | y' <- [b .. b']]
      get_paths (a, b, '<') (_, b') = [(a, y') | y' <- [b' .. b]]
      get_paths (_, _, _) (_, _) = []
      edge '^' = (0, y)
      edge '>' = (x, terrain_size)
      edge 'v' = (terrain_size, y)
      edge '<' = (x, 0)
      edge _ = (0, 0)
   in case newsp of
        Nothing -> nub $ walked ++ get_paths sp (edge dir)
        Just nsp@(x', y', _) -> get_walked_list nsp ter (walked ++ get_paths sp (x', y'))

get_next_obstacle terrain (x, y, dir) =
  case possible_obsts dir of
    h : _ -> Just h
    _ -> Nothing
  where
    possible_obsts '^' = [(a, y, '^') | a <- reverse [0 .. x], M.lookup (a, y) terrain == Just '#']
    possible_obsts 'v' = [(a, y, 'v') | a <- [x .. terrain_size], M.lookup (a, y) terrain == Just '#']
    possible_obsts '<' = [(x, a, '<') | a <- reverse [0 .. y], M.lookup (x, a) terrain == Just '#']
    possible_obsts '>' = [(x, a, '>') | a <- [y .. terrain_size], M.lookup (x, a) terrain == Just '#']
    possible_obsts _ = []
    terrain_size = fst . fst $ M.findMax terrain

new_starting_point (x, y, '^') = (x + 1, y, '>')
new_starting_point (x, y, 'v') = (x - 1, y, '<')
new_starting_point (x, y, '>') = (x, y - 1, 'v')
new_starting_point (x, y, '<') = (x, y + 1, '^')
new_starting_point (x, y, c) = (x, y, c)

loops sp obslist ter =
  case next_obst of
    Nothing -> False
    Just p@(x, y, dir) -> ((x, y, dir) `elem` obslist) || loops (new_starting_point p) ((x, y, dir) : obslist) ter
  where
    next_obst = get_next_obstacle ter sp