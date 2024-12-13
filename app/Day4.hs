{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Day4 where

import Data.Foldable (Foldable (foldl'), find)

data Point = Point Int Int Char deriving (Show, Eq)

type Grid = [Point]

find_point x y grid
  | x > 0 && y > 0 =
      find (\(Point a b _) -> a == x && b == y) grid
  | otherwise = Nothing

get_char_of_point (Point _ _ c) = c

row_to_grid row_id chars = reverse $ go 1 [] chars
  where
    go _ acc [] = acc
    go x acc (h : rest) = go (x + 1) (Point x row_id h : acc) rest

input_to_grid str = concat $ go 1 [] $ lines str
  where
    go _ acc [] = acc
    go row_id acc (h : rest) = go (row_id + 1) (reverse (row_to_grid row_id h : acc)) rest

is_xmas_right (Point x y _) grid =
  let m = find_point (x + 1) y grid
      a = find_point (x + 2) y grid
      s = find_point (x + 3) y grid
   in case (m, a, s) of
        (Just (Point _ _ 'M'), Just (Point _ _ 'A'), Just (Point _ _ 'S')) -> True
        _ -> False

is_xmas_left (Point x y _) grid =
  let m = find_point (x - 1) y grid
      a = find_point (x - 2) y grid
      s = find_point (x - 3) y grid
   in case (m, a, s) of
        (Just (Point _ _ 'M'), Just (Point _ _ 'A'), Just (Point _ _ 'S')) -> True
        _ -> False

is_xmas_top (Point x y _) grid =
  let m = find_point x (y - 1) grid
      a = find_point x (y - 2) grid
      s = find_point x (y - 3) grid
   in case (m, a, s) of
        (Just (Point _ _ 'M'), Just (Point _ _ 'A'), Just (Point _ _ 'S')) -> True
        _ -> False

is_xmas_bottom (Point x y _) grid =
  let m = find_point x (y + 1) grid
      a = find_point x (y + 2) grid
      s = find_point x (y + 3) grid
   in case (m, a, s) of
        (Just (Point _ _ 'M'), Just (Point _ _ 'A'), Just (Point _ _ 'S')) -> True
        _ -> False

is_xmas_top_left (Point x y _) grid =
  let m = find_point (x - 1) (y - 1) grid
      a = find_point (x - 2) (y - 2) grid
      s = find_point (x - 3) (y - 3) grid
   in case (m, a, s) of
        (Just (Point _ _ 'M'), Just (Point _ _ 'A'), Just (Point _ _ 'S')) -> True
        _ -> False

is_xmas_top_right (Point x y _) grid =
  let m = find_point (x + 1) (y - 1) grid
      a = find_point (x + 2) (y - 2) grid
      s = find_point (x + 3) (y - 3) grid
   in case (m, a, s) of
        (Just (Point _ _ 'M'), Just (Point _ _ 'A'), Just (Point _ _ 'S')) -> True
        _ -> False

is_xmas_bottom_left (Point x y _) grid =
  let m = find_point (x - 1) (y + 1) grid
      a = find_point (x - 2) (y + 2) grid
      s = find_point (x - 3) (y + 3) grid
   in case (m, a, s) of
        (Just (Point _ _ 'M'), Just (Point _ _ 'A'), Just (Point _ _ 'S')) -> True
        _ -> False

is_xmas_bottom_right (Point x y _) grid =
  let m = find_point (x + 1) (y + 1) grid
      a = find_point (x + 2) (y + 2) grid
      s = find_point (x + 3) (y + 3) grid
   in case (m, a, s) of
        (Just (Point _ _ 'M'), Just (Point _ _ 'A'), Just (Point _ _ 'S')) -> True
        _ -> False

count_nearby_xmas point grid =
  let t = if is_xmas_top point grid then 1 else 0
      r = if is_xmas_right point grid then 1 else 0
      b = if is_xmas_bottom point grid then 1 else 0
      l = if is_xmas_left point grid then 1 else 0
      tl = if is_xmas_top_left point grid then 1 else 0
      tr = if is_xmas_top_right point grid then 1 else 0
      bl = if is_xmas_bottom_left point grid then 1 else 0
      br = if is_xmas_bottom_right point grid then 1 else 0
   in t + r + b + l + tl + tr + bl + br

count_xmas grid = foldl' f 0 grid
  where
    f total point@(Point _ _ 'X') = total + count_nearby_xmas point grid
    f total _ = total

get_adjacents x y grid =
  let tl = find_point (x - 1) (y - 1) grid
      tr = find_point (x + 1) (y - 1) grid
      bl = find_point (x - 1) (y + 1) grid
      br = find_point (x + 1) (y + 1) grid
   in case (tl, tr, bl, br) of
        (Just (Point _ _ a), Just (Point _ _ b), Just (Point _ _ c), Just (Point _ _ d)) -> [a, b, c, d]
        _ -> ""

is_valid_xmas (Point a b 'A') grid = get_adjacents a b grid `elem` ["MSMS", "SMSM", "SSMM", "MMSS"]
is_valid_xmas _ _ = False

count_xmas2 grid = foldl' f 0 grid
  where
    f total point@(Point _ _ 'A') = total + (if is_valid_xmas point grid then 1 else 0)
    f total _ = total

get_grid_from_input = readFile "app/day4input.txt" >>= pure . input_to_grid

part_1 = do
  grid <- get_grid_from_input
  return $ count_xmas grid :: IO Int

part_2 = do
  grid <- get_grid_from_input
  return $ count_xmas2 grid :: IO Int