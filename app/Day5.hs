{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Day5 where

import Data.Foldable (Foldable (foldl'))
import Data.List (intercalate)
import Data.List.Split

read_input = do
  rules <- readFile "app/day5input-rules.txt" >>= pure . lines
  updates <- readFile "app/day5input-updates.txt" >>= pure . lines
  return (rules, updates)

mk_update_pairs updates_str = reverse $ go [] $ splitOn "," updates_str
  where
    go acc (a : b : rest) = go ((a ++ "|" ++ b) : acc) (b : rest)
    go acc _ = acc

is_valid_update rules update_line =
  foldl' f True (mk_update_pairs update_line)
  where
    f bool update_pair = bool && update_pair `elem` rules

get_middle update_line =
  let nums_list = map (read :: String -> Int) (splitOn "," update_line)
   in nums_list !! (length nums_list `div` 2)

part_1 = do
  (rules_list, updates_list) <- read_input
  return $ sum $ map get_middle $ filter (is_valid_update rules_list) updates_list

mk_update_line_valid rules update_line
  | is_valid_update rules update_line = update_line
  | otherwise = fix_line [] (splitOn "," update_line)
  where
    fix_line acc (a : b : rest) = fix_line (acc ++ [fst (fixed a b)]) (snd (fixed a b) : rest)
    fix_line acc [x] = mk_update_line_valid rules $ join_line (acc ++ [x])
    fix_line acc [] = mk_update_line_valid rules $ join_line acc

    fixed a b = if (a ++ "|" ++ b) `elem` rules then (a, b) else (b, a)

    join_line = intercalate ","

part_2 = do
  (rules_list, updates_list) <- read_input
  return $ sum $ map (get_middle . mk_update_line_valid rules_list) $ filter (not . is_valid_update rules_list) updates_list
