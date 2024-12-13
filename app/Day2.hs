{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Day2 where

data Trend = Decreasing | Increasing | Invalid deriving (Eq, Show)

get_trend (f : s : _)
  | f < s = Increasing
  | f > s = Decreasing
  | otherwise = Invalid
get_trend _ = Invalid

is_valid (a, b) trend
  | trend == Decreasing = a > b && (a - b) <= 3
  | trend == Increasing = a < b && (b - a) <= 3
  | otherwise = False

pair_up xs = go [] xs
  where
    go acc (f : s : rest) = go (acc ++ [(f, s)]) (s : rest)
    go acc _ = acc

is_safe_level level =
  let trend = get_trend level
      pairs = pair_up level
   in foldl (f trend) True pairs
  where
    f t b pair = b && is_valid pair t

is_safe_level' is_top_level level =
  let trend = get_trend level
      pairs = pair_up level
      top_level_res = foldl (f trend) True pairs
   in (top_level_res || (is_top_level && foldl f' False (mk_levels level)))
  where
    f t b pair = b && is_valid pair t
    f' b l = b || is_safe_level' False l

mk_levels level =
  let lw_idx = foldl (\xs l -> xs ++ [(length xs, l)]) [] level
   in foldl (\acc l -> acc ++ [map snd (filter (/= l) lw_idx)]) [] lw_idx

part_1 = do
  input <- readFile "app/day2input.txt"
  let ls = lines input
      ws = map (map read . words) ls :: [[Int]]
  return $ length $ filter is_safe_level ws

part_2 = do
  input <- readFile "app/day2input.txt"
  let ls = lines input
      ws = map (map read . words) ls :: [[Int]]
  return $ length $ filter (is_safe_level' True) ws