module Day7 where

import Control.Monad (replicateM)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)

readinput = do
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

reduceline _ [] _ = False
reduceline target [x] _ = target == x
reduceline _ _ [] = False
reduceline target (a : b : rest) (f : rest') = (f a b <= target) && reduceline target (f a b : rest) rest'

join' a b = read (show a ++ show b) :: Int

-- testline (result, nums) =
--   let possResults = mapMaybe (reduceline result nums) (combinations (length nums - 1) [(*), (+)])
--    in result `elem` possResults

-- testline2 (result, nums) =
--   let possResults = mapMaybe (reduceline result nums) (combinations (length nums - 1) [(*), (+), concat'])
--       concat' a b = read $ show a ++ show b :: Int
--    in result `elem` possResults

part1 = do
  inputs <- readinput
  let corrects = filter (\(t, nums) -> let opcombs = combinations nums [(*), (+)] in foldl (\bool ops -> bool || reduceline t nums ops) False opcombs) inputs
  pure $ sum $ map fst corrects

part2 = do
  inputs <- readinput
  let corrects = filter (\(t, nums) -> let opcombs = combinations nums [(*), (+), join'] in foldl (\bool ops -> bool || reduceline t nums ops) False opcombs) inputs
  pure $ sum $ map fst corrects