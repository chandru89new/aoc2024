module Day2 where

minDiff = 1

maxDiff = 3

data Trend = Decreasing | Increasing | Invalid deriving (Eq, Show)

getTrend (f : s : _)
  | f < s = Increasing
  | f > s = Decreasing
  | otherwise = Invalid
getTrend _ = Invalid

isValid (a, b) trend
  | trend == Decreasing = a > b && (a - b) <= 3
  | trend == Increasing = a < b && (b - a) <= 3
  | otherwise = False

pairUp xs = go [] xs
  where
    go acc (f : s : rest) = go (acc ++ [(f, s)]) (s : rest)
    go acc _ = acc

isSafeLevel level =
  let trend = getTrend level
      pairs = pairUp level
   in foldl (f trend) True pairs
  where
    f t b pair = b && isValid pair t

isSafeLevel' isTopLevel level =
  let trend = getTrend level
      pairs = pairUp level
      topLevelRes = foldl (f trend) True pairs
   in if topLevelRes
        then True
        else
          if not isTopLevel
            then False
            else foldl f' False (mkLevels level)
  where
    f t b pair = b && isValid pair t
    f' b l = b || isSafeLevel' False l

mkLevels level =
  let lwidx = foldl (\xs l -> xs ++ [(length xs, l)]) [] level
   in foldl (\acc l -> acc ++ [map snd (filter (/= l) lwidx)]) [] lwidx

-- [1,2,3,4,5]
-- [(0,1),(1,2)..]
-- [[(1,2),(2,3)..],[(0,1),..],[...]]
-- [[2,3..],[]]

part1 = do
  input <- readFile "app/day2input.txt"
  let ls = lines input
      ws = map (map read . words) ls :: [[Int]]
  return $ length $ filter isSafeLevel ws

part2 = do
  input <- readFile "app/day2input.txt"
  let ls = lines input
      ws = map (map read . words) ls :: [[Int]]
  return $ length $ filter (isSafeLevel' True) ws