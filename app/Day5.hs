module Day5 where

import Data.Foldable (Foldable (foldl'))
import Data.List (intercalate)
import Data.List.Split

readInput = do
  rules <- readFile "app/day5input-rules.txt" >>= pure . lines
  updates <- readFile "app/day5input-updates.txt" >>= pure . lines
  return (rules, updates)

mkUpdatePairs updatesStr = reverse $ go [] $ splitOn "," updatesStr
  where
    go acc (a : b : rest) = go ((a ++ "|" ++ b) : acc) (b : rest)
    go acc _ = acc

isValidUpdate rules updateLine =
  foldl' f True (mkUpdatePairs updateLine)
  where
    f bool updatePair = bool && updatePair `elem` rules

getMiddle updateLine =
  let numsList = map (read :: String -> Int) (splitOn "," updateLine)
   in numsList !! (length numsList `div` 2)

part1 = do
  (rulesList, updatesList) <- readInput
  return $ sum $ map getMiddle $ filter (isValidUpdate rulesList) updatesList

mkUpdateLineValid rules updateLine
  | isValidUpdate rules updateLine = updateLine
  | otherwise = fixLine [] (splitOn "," updateLine)
  where
    fixLine acc (a : b : rest) = fixLine (acc ++ [fst (fixed a b)]) (snd (fixed a b) : rest)
    fixLine acc [x] = mkUpdateLineValid rules $ joinLine (acc ++ [x])
    fixLine acc [] = mkUpdateLineValid rules $ joinLine acc

    fixed a b = if (a ++ "|" ++ b) `elem` rules then (a, b) else (b, a)

    joinLine = intercalate ","

part2 = do
  (rulesList, updatesList) <- readInput
  return $ sum $ map (getMiddle . mkUpdateLineValid rulesList) $ filter (not . isValidUpdate rulesList) updatesList
