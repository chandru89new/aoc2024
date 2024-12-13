{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Day1 where

import Data.List

tuple_sort :: ([Int], [Int]) -> ([Int], [Int])
tuple_sort (as, bs) = (sort as, sort bs)

as_pairs (as, bs) = go [] as bs
  where
    go acc [] _ = acc
    go acc _ [] = acc
    go acc (a : as') (b : bs') = go ((a, b) : acc) as' bs'

diffs xs = go [] xs
  where
    go acc [] = acc
    go acc ((a, b) : rest) = go (abs (a - b) : acc) rest

get_sim_core xs x = (* x) $ length $ filter (== x) xs

sim_scores (as, bs) = map (get_sim_core bs) as

part_1 = do
  input <- readFile "app/day1input.txt"
  return
    $ sum
    $ diffs
    $ as_pairs
    $ tuple_sort
    $ foldl
      ( \(as, bs) ns -> case map read ns of
          (a : b : _) -> (a : as, b : bs)
          _ -> (as, bs)
      )
      ([], [])
    $ map words
    $ lines input

part_2 = do
  input <- readFile "app/day1input.txt"
  return
    $ sum
    $ sim_scores
    $ tuple_sort
    $ foldl
      ( \(as, bs) ns -> case map read ns of
          (a : b : _) -> (a : as, b : bs)
          _ -> (as, bs)
      )
      ([], [])
    $ map words
    $ lines input