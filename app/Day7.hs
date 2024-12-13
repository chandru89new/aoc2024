{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Day7 where

import Control.Monad (replicateM)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)

read_input = do
  input <- readFile "app/day7input.txt"
  let aslines = lines input
      linessplit =
        map
          ( \l -> case splitOn ":" l of
              [h, t] -> Just (read h, map read $ words t) :: Maybe (Int, [Int])
              _ -> Nothing
          )
          aslines
  pure $ catMaybes linessplit

combinations ns = replicateM (length ns - 1)

reduce_line _ [] _ = False
reduce_line target [x] _ = target == x
reduce_line _ _ [] = False
reduce_line target (a : b : rest) (f : rest') = (f a b <= target) && reduce_line target (f a b : rest) rest'

join' a b = read (show a ++ show b) :: Int

part_1 = do
  inputs <- read_input
  let corrects = filter (\(t, nums) -> let opcombs = combinations nums [(*), (+)] in foldl (\bool ops -> bool || reduce_line t nums ops) False opcombs) inputs
  pure $ sum $ map fst corrects

part_2 = do
  inputs <- read_input
  let corrects = filter (\(t, nums) -> let opcombs = combinations nums [(*), (+), join'] in foldl (\bool ops -> bool || reduce_line t nums ops) False opcombs) inputs
  pure $ sum $ map fst corrects