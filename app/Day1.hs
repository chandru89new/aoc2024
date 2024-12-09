module Day1 where

import Data.List

tuplesort :: ([Int], [Int]) -> ([Int], [Int])
tuplesort (as, bs) = (sort as, sort bs)

aspairs (as, bs) = go [] as bs
  where
    go acc [] _ = acc
    go acc _ [] = acc
    go acc (a : as') (b : bs') = go ((a, b) : acc) as' bs'

diffs xs = go [] xs
  where
    go acc [] = acc
    go acc ((a, b) : rest) = go (abs (a - b) : acc) rest

getsimcore xs x = (* x) $ length $ filter (== x) xs

simscores (as, bs) = map (getsimcore bs) as

part1 = do
  input <- readFile "app/day1input.txt"
  return
    $ sum
    $ diffs
    $ aspairs
    $ tuplesort
    $ foldl
      ( \(as, bs) ns -> case map read ns of
          (a : b : _) -> (a : as, b : bs)
          _ -> (as, bs)
      )
      ([], [])
    $ map words
    $ lines input

part2 = do
  input <- readFile "app/day1input.txt"
  return
    $ sum
    $ simscores
    $ tuplesort
    $ foldl
      ( \(as, bs) ns -> case map read ns of
          (a : b : _) -> (a : as, b : bs)
          _ -> (as, bs)
      )
      ([], [])
    $ map words
    $ lines input