{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use <$>" #-}
module Day11 where

import Data.Foldable (Foldable (foldl'))
import qualified Data.Map as M

part_1 = do
  input <- read_input
  pure $ total_stones $ blink_n 25 input

part_2 :: IO Int
part_2 = do
  input <- read_input
  pure $ total_stones $ blink_n 75 input

apply_for_0 = 1

apply_for_even num = map (read :: String -> Int) $ split_half (show num)

split_half xs = [take (length xs `div` 2) xs, drop (length xs `div` 2) xs]

apply_for_others n = n * 2024

apply_rule :: Int -> [Int]
apply_rule n
  | n == 0 = [apply_for_0]
  | isEvenDigited n = apply_for_even n
  | otherwise = [apply_for_others n]

isEvenDigited n = even (length (show n))

type Stones = M.Map Int Int

read_input = do
  input <- readFile "app/day11input.txt"
  pure $ foldl' f M.empty $ map (read :: String -> Int) $ words input
  where
    f m n = M.insert n 1 m

curr_state :: Stones -> [(Int, Int)]
curr_state = M.assocs

spawns :: (Int, Int) -> [(Int, Int)]
spawns (n, c) = map (\n1 -> (n1, c)) (apply_rule n)

blink :: Stones -> Stones
blink stones =
  let cs = curr_state stones
      is = concatMap spawns cs
   in create_stone_map is

create_stone_map :: [(Int, Int)] -> Stones
create_stone_map = foldl' f M.empty
  where
    f m (k, v) = case M.lookup k m of
      Nothing -> M.insert k v m
      Just v' -> M.update (\_ -> Just $ v + v') k m

blink_n :: Int -> Stones -> Stones
blink_n 0 s = s
blink_n c s = blink_n (c - 1) (blink s)

total_stones :: Stones -> Int
total_stones stones = sum $ map snd $ M.assocs stones