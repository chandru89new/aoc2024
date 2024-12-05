module Day4 where

import Data.Foldable (Foldable (foldl'), find)

data Point = Point Int Int Char deriving (Show, Eq)

type Grid = [Point]

findPoint x y grid
  | x > 0 && y > 0 =
      find (\(Point a b _) -> a == x && b == y) grid
  | otherwise = Nothing

getCharOfPoint (Point _ _ c) = c

rowToGrid rowId chars = reverse $ go 1 [] chars
  where
    go _ acc [] = acc
    go x acc (h : rest) = go (x + 1) (Point x rowId h : acc) rest

inputToGrid str = concat $ go 1 [] $ lines str
  where
    go _ acc [] = acc
    go rowId acc (h : rest) = go (rowId + 1) (reverse (rowToGrid rowId h : acc)) rest

isXmasRight (Point x y _) grid =
  let m = findPoint (x + 1) y grid
      a = findPoint (x + 2) y grid
      s = findPoint (x + 3) y grid
   in case (m, a, s) of
        (Just (Point _ _ 'M'), Just (Point _ _ 'A'), Just (Point _ _ 'S')) -> True
        _ -> False

isXmasLeft (Point x y _) grid =
  let m = findPoint (x - 1) y grid
      a = findPoint (x - 2) y grid
      s = findPoint (x - 3) y grid
   in case (m, a, s) of
        (Just (Point _ _ 'M'), Just (Point _ _ 'A'), Just (Point _ _ 'S')) -> True
        _ -> False

isXmasTop (Point x y _) grid =
  let m = findPoint x (y - 1) grid
      a = findPoint x (y - 2) grid
      s = findPoint x (y - 3) grid
   in case (m, a, s) of
        (Just (Point _ _ 'M'), Just (Point _ _ 'A'), Just (Point _ _ 'S')) -> True
        _ -> False

isXmasBottom (Point x y _) grid =
  let m = findPoint x (y + 1) grid
      a = findPoint x (y + 2) grid
      s = findPoint x (y + 3) grid
   in case (m, a, s) of
        (Just (Point _ _ 'M'), Just (Point _ _ 'A'), Just (Point _ _ 'S')) -> True
        _ -> False

isXmasTopLeft (Point x y _) grid =
  let m = findPoint (x - 1) (y - 1) grid
      a = findPoint (x - 2) (y - 2) grid
      s = findPoint (x - 3) (y - 3) grid
   in case (m, a, s) of
        (Just (Point _ _ 'M'), Just (Point _ _ 'A'), Just (Point _ _ 'S')) -> True
        _ -> False

isXmasTopRight (Point x y _) grid =
  let m = findPoint (x + 1) (y - 1) grid
      a = findPoint (x + 2) (y - 2) grid
      s = findPoint (x + 3) (y - 3) grid
   in case (m, a, s) of
        (Just (Point _ _ 'M'), Just (Point _ _ 'A'), Just (Point _ _ 'S')) -> True
        _ -> False

isXmasBottomLeft (Point x y _) grid =
  let m = findPoint (x - 1) (y + 1) grid
      a = findPoint (x - 2) (y + 2) grid
      s = findPoint (x - 3) (y + 3) grid
   in case (m, a, s) of
        (Just (Point _ _ 'M'), Just (Point _ _ 'A'), Just (Point _ _ 'S')) -> True
        _ -> False

isXmasBottomRight (Point x y _) grid =
  let m = findPoint (x + 1) (y + 1) grid
      a = findPoint (x + 2) (y + 2) grid
      s = findPoint (x + 3) (y + 3) grid
   in case (m, a, s) of
        (Just (Point _ _ 'M'), Just (Point _ _ 'A'), Just (Point _ _ 'S')) -> True
        _ -> False

countNearbyXmas point grid =
  let t = if isXmasTop point grid then 1 else 0
      r = if isXmasRight point grid then 1 else 0
      b = if isXmasBottom point grid then 1 else 0
      l = if isXmasLeft point grid then 1 else 0
      tl = if isXmasTopLeft point grid then 1 else 0
      tr = if isXmasTopRight point grid then 1 else 0
      bl = if isXmasBottomLeft point grid then 1 else 0
      br = if isXmasBottomRight point grid then 1 else 0
   in t + r + b + l + tl + tr + bl + br

countXmas grid = foldl' f 0 grid
  where
    f total point@(Point _ _ 'X') = total + countNearbyXmas point grid
    f total _ = total

-- MSMS, SMSM, SSMM, MMSS
getAdjacents x y grid =
  let tl = findPoint (x - 1) (y - 1) grid
      tr = findPoint (x + 1) (y - 1) grid
      bl = findPoint (x - 1) (y + 1) grid
      br = findPoint (x + 1) (y + 1) grid
   in case (tl, tr, bl, br) of
        (Just (Point _ _ a), Just (Point _ _ b), Just (Point _ _ c), Just (Point _ _ d)) -> [a, b, c, d]
        _ -> ""

isValidXmas (Point a b 'A') grid = getAdjacents a b grid `elem` ["MSMS", "SMSM", "SSMM", "MMSS"]
isValidXmas _ _ = False

countXmas2 grid = foldl' f 0 grid
  where
    f total point@(Point _ _ 'A') = total + (if isValidXmas point grid then 1 else 0)
    f total _ = total

getGridFromInput = readFile "app/day4input.txt" >>= pure . inputToGrid

part1 = do
  grid <- getGridFromInput
  return $ countXmas grid :: IO Int

part2 = do
  grid <- getGridFromInput
  return $ countXmas2 grid :: IO Int