module Day6 where

import Control.Concurrent.Async (mapConcurrently)
import Data.List (nub)
import qualified Data.Map as M
import Utils (foldinput)

readinput = do
  input <- readFile "app/day6input.txt"
  pure $ foldinput ((0, 0, '>'), M.empty) f input
  where
    f x y c (g, terrain) = (if c `elem` ['^', '>', 'v', '<'] then (x, y, c) else g, M.insert (x, y) c terrain)

part1 = do
  (sp, ter) <- readinput
  pure $ length $ getwalkedlist sp ter []

part2 = do
  (sp, ter) <- readinput
  let newmaps = map (\w -> M.insert w '#' ter) $ getwalkedlist sp ter []
  res <- mapConcurrently (pure . loops sp []) newmaps
  pure $ length $ filter id res

getwalkedlist sp@(x, y, dir) ter walked =
  let firstobst = getnextobstacle ter sp
      newsp = newstartingpoint <$> firstobst
      terrainsize = fst . fst $ M.findMax ter
      getpaths (a, b, '^') (a', _) = [(x', b) | x' <- [a' .. a]]
      getpaths (a, b, 'v') (a', _) = [(x', b) | x' <- [a .. a']]
      getpaths (a, b, '>') (_, b') = [(a, y') | y' <- [b .. b']]
      getpaths (a, b, '<') (_, b') = [(a, y') | y' <- [b' .. b]]
      getpaths (_, _, _) (_, _) = []
      edge '^' = (0, y)
      edge '>' = (x, terrainsize)
      edge 'v' = (terrainsize, y)
      edge '<' = (x, 0)
      edge _ = (0, 0)
   in case newsp of
        Nothing -> nub $ walked ++ getpaths sp (edge dir)
        Just nsp@(x', y', _) -> getwalkedlist nsp ter (walked ++ getpaths sp (x', y'))

getnextobstacle terrain (x, y, dir) =
  case possibleobsts dir of
    h : _ -> Just h
    _ -> Nothing
  where
    possibleobsts '^' = [(a, y, '^') | a <- reverse [0 .. x], M.lookup (a, y) terrain == Just '#']
    possibleobsts 'v' = [(a, y, 'v') | a <- [x .. terrainsize], M.lookup (a, y) terrain == Just '#']
    possibleobsts '<' = [(x, a, '<') | a <- reverse [0 .. y], M.lookup (x, a) terrain == Just '#']
    possibleobsts '>' = [(x, a, '>') | a <- [y .. terrainsize], M.lookup (x, a) terrain == Just '#']
    possibleobsts _ = []
    terrainsize = fst . fst $ M.findMax terrain

newstartingpoint (x, y, '^') = (x + 1, y, '>')
newstartingpoint (x, y, 'v') = (x - 1, y, '<')
newstartingpoint (x, y, '>') = (x, y - 1, 'v')
newstartingpoint (x, y, '<') = (x, y + 1, '^')
newstartingpoint (x, y, c) = (x, y, c)

loops sp obslist ter =
  case nextobst of
    Nothing -> False
    Just p@(x, y, dir) -> ((x, y, dir) `elem` obslist) || loops (newstartingpoint p) ((x, y, dir) : obslist) ter
  where
    nextobst = getnextobstacle ter sp