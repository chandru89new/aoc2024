module Day2 where

data Trend = Decreasing | Increasing | Invalid deriving (Eq, Show)

gettrend (f : s : _)
  | f < s = Increasing
  | f > s = Decreasing
  | otherwise = Invalid
gettrend _ = Invalid

isvalid (a, b) trend
  | trend == Decreasing = a > b && (a - b) <= 3
  | trend == Increasing = a < b && (b - a) <= 3
  | otherwise = False

pairup xs = go [] xs
  where
    go acc (f : s : rest) = go (acc ++ [(f, s)]) (s : rest)
    go acc _ = acc

issafelevel level =
  let trend = gettrend level
      pairs = pairup level
   in foldl (f trend) True pairs
  where
    f t b pair = b && isvalid pair t

issafelevel' istoplevel level =
  let trend = gettrend level
      pairs = pairup level
      toplevelres = foldl (f trend) True pairs
   in (toplevelres || (istoplevel && foldl f' False (mklevels level)))
  where
    f t b pair = b && isvalid pair t
    f' b l = b || issafelevel' False l

mklevels level =
  let lwidx = foldl (\xs l -> xs ++ [(length xs, l)]) [] level
   in foldl (\acc l -> acc ++ [map snd (filter (/= l) lwidx)]) [] lwidx

part1 = do
  input <- readFile "app/day2input.txt"
  let ls = lines input
      ws = map (map read . words) ls :: [[Int]]
  return $ length $ filter issafelevel ws

part2 = do
  input <- readFile "app/day2input.txt"
  let ls = lines input
      ws = map (map read . words) ls :: [[Int]]
  return $ length $ filter (issafelevel' True) ws