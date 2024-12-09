module Day5 where

import Data.Foldable (Foldable (foldl'))
import Data.List (intercalate)
import Data.List.Split

readinput = do
  rules <- readFile "app/day5input-rules.txt" >>= pure . lines
  updates <- readFile "app/day5input-updates.txt" >>= pure . lines
  return (rules, updates)

mkupdatepairs updatesstr = reverse $ go [] $ splitOn "," updatesstr
  where
    go acc (a : b : rest) = go ((a ++ "|" ++ b) : acc) (b : rest)
    go acc _ = acc

isvalidupdate rules updateline =
  foldl' f True (mkupdatepairs updateline)
  where
    f bool updatepair = bool && updatepair `elem` rules

getmiddle updateline =
  let numslist = map (read :: String -> Int) (splitOn "," updateline)
   in numslist !! (length numslist `div` 2)

part1 = do
  (ruleslist, updateslist) <- readinput
  return $ sum $ map getmiddle $ filter (isvalidupdate ruleslist) updateslist

mkupdatelinevalid rules updateline
  | isvalidupdate rules updateline = updateline
  | otherwise = fixline [] (splitOn "," updateline)
  where
    fixline acc (a : b : rest) = fixline (acc ++ [fst (fixed a b)]) (snd (fixed a b) : rest)
    fixline acc [x] = mkupdatelinevalid rules $ joinline (acc ++ [x])
    fixline acc [] = mkupdatelinevalid rules $ joinline acc

    fixed a b = if (a ++ "|" ++ b) `elem` rules then (a, b) else (b, a)

    joinline = intercalate ","

part2 = do
  (ruleslist, updateslist) <- readinput
  return $ sum $ map (getmiddle . mkupdatelinevalid ruleslist) $ filter (not . isvalidupdate ruleslist) updateslist
