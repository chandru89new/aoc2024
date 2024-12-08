module Day6 where

import Control.Concurrent.Async (mapConcurrently)
import Data.List (nub)
import qualified Data.Map as M
import Utils (foldInput)

readInput = do
  input <- readFile "app/day6input.txt"
  pure $ foldInput ((0, 0, '>'), M.empty) f input
  where
    f x y c (g, terrain) = (if c `elem` ['^', '>', 'v', '<'] then (x, y, c) else g, M.insert (x, y) c terrain)

part1 = do
  (sp, ter) <- readInput
  print . length $ getwalkedlist sp ter []

part2 = do
  (sp, ter) <- readInput
  let newMaps = map (\w -> M.insert w '#' ter) $ getwalkedlist sp ter []
  res <- mapConcurrently (pure . loops sp []) newMaps
  pure $ length $ filter id res

getwalkedlist sp@(x, y, dir) ter walked =
  let firstobst = getNextObstacle ter sp
      newsp = newStartingPoint <$> firstobst
      terrainSize = fst . fst $ M.findMax ter
      getPaths (a, b, '^') (a', _) = [(x', b) | x' <- [a' .. a]]
      getPaths (a, b, 'v') (a', _) = [(x', b) | x' <- [a .. a']]
      getPaths (a, b, '>') (_, b') = [(a, y') | y' <- [b .. b']]
      getPaths (a, b, '<') (_, b') = [(a, y') | y' <- [b' .. b]]
      getPaths (_, _, _) (_, _) = []
      edge '^' = (0, y)
      edge '>' = (x, terrainSize)
      edge 'v' = (terrainSize, y)
      edge '<' = (x, 0)
      edge _ = (0, 0)
   in case newsp of
        Nothing -> nub $ walked ++ getPaths sp (edge dir)
        Just nsp@(x', y', _) -> getwalkedlist nsp ter (walked ++ getPaths sp (x', y'))

getNextObstacle terrain (x, y, dir) =
  case possibleObsts dir of
    h : _ -> Just h
    _ -> Nothing
  where
    possibleObsts '^' = [(a, y, '^') | a <- reverse [0 .. x], M.lookup (a, y) terrain == Just '#']
    possibleObsts 'v' = [(a, y, 'v') | a <- [x .. terrainSize], M.lookup (a, y) terrain == Just '#']
    possibleObsts '<' = [(x, a, '<') | a <- reverse [0 .. y], M.lookup (x, a) terrain == Just '#']
    possibleObsts '>' = [(x, a, '>') | a <- [y .. terrainSize], M.lookup (x, a) terrain == Just '#']
    possibleObsts _ = []
    terrainSize = fst . fst $ M.findMax terrain

newStartingPoint (x, y, '^') = (x + 1, y, '>')
newStartingPoint (x, y, 'v') = (x - 1, y, '<')
newStartingPoint (x, y, '>') = (x, y - 1, 'v')
newStartingPoint (x, y, '<') = (x, y + 1, '^')
newStartingPoint (x, y, c) = (x, y, c)

loops sp obsList ter =
  case nextObst of
    Nothing -> False
    Just p@(x, y, dir) -> ((x, y, dir) `elem` obsList) || loops (newSp p) ((x, y, dir) : obsList) ter
  where
    nextObst = getNextObstacle ter sp
    newSp = newStartingPoint