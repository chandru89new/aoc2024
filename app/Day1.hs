module Day1 where

import Data.List

tupleSort :: ([Int], [Int]) -> ([Int], [Int])
tupleSort (as, bs) = (sort as, sort bs)

asPairs (as, bs) = go [] as bs
  where
    go acc [] _ = acc
    go acc _ [] = acc
    go acc (a : as') (b : bs') = go ((a, b) : acc) as' bs'

diffs xs = go [] xs
  where
    go acc [] = acc
    go acc ((a, b) : rest) = go (abs (a - b) : acc) rest

part1 = do
  input <- readFile "app/day1input.txt"
  return
    $ sum
    $ diffs
    $ asPairs
    $ tupleSort
    $ foldl
      ( \(as, bs) ns -> case map read ns of
          (a : b : _) -> (a : as, b : bs)
          _ -> (as, bs)
      )
      ([], [])
    $ map words
    $ lines input

getSimScore xs x = (* x) $ length $ filter (== x) xs

simScores (as, bs) = map (getSimScore bs) as

part2 = do
  input <- readFile "app/day1input.txt"
  return
    $ sum
    $ simScores
    $ tupleSort
    $ foldl
      ( \(as, bs) ns -> case map read ns of
          (a : b : _) -> (a : as, b : bs)
          _ -> (as, bs)
      )
      ([], [])
    $ map words
    $ lines input