module Day4 where

import Data.Foldable (Foldable (foldl'), find)

data Point = Point Int Int Char deriving (Show, Eq)

type Grid = [Point]

findpoint x y grid
  | x > 0 && y > 0 =
      find (\(Point a b _) -> a == x && b == y) grid
  | otherwise = Nothing

getcharofpoint (Point _ _ c) = c

rowtogrid rowid chars = reverse $ go 1 [] chars
  where
    go _ acc [] = acc
    go x acc (h : rest) = go (x + 1) (Point x rowid h : acc) rest

inputtogrid str = concat $ go 1 [] $ lines str
  where
    go _ acc [] = acc
    go rowid acc (h : rest) = go (rowid + 1) (reverse (rowtogrid rowid h : acc)) rest

isxmasright (Point x y _) grid =
  let m = findpoint (x + 1) y grid
      a = findpoint (x + 2) y grid
      s = findpoint (x + 3) y grid
   in case (m, a, s) of
        (Just (Point _ _ 'M'), Just (Point _ _ 'A'), Just (Point _ _ 'S')) -> True
        _ -> False

isxmasleft (Point x y _) grid =
  let m = findpoint (x - 1) y grid
      a = findpoint (x - 2) y grid
      s = findpoint (x - 3) y grid
   in case (m, a, s) of
        (Just (Point _ _ 'M'), Just (Point _ _ 'A'), Just (Point _ _ 'S')) -> True
        _ -> False

isxmastop (Point x y _) grid =
  let m = findpoint x (y - 1) grid
      a = findpoint x (y - 2) grid
      s = findpoint x (y - 3) grid
   in case (m, a, s) of
        (Just (Point _ _ 'M'), Just (Point _ _ 'A'), Just (Point _ _ 'S')) -> True
        _ -> False

isxmasbottom (Point x y _) grid =
  let m = findpoint x (y + 1) grid
      a = findpoint x (y + 2) grid
      s = findpoint x (y + 3) grid
   in case (m, a, s) of
        (Just (Point _ _ 'M'), Just (Point _ _ 'A'), Just (Point _ _ 'S')) -> True
        _ -> False

isxmastopleft (Point x y _) grid =
  let m = findpoint (x - 1) (y - 1) grid
      a = findpoint (x - 2) (y - 2) grid
      s = findpoint (x - 3) (y - 3) grid
   in case (m, a, s) of
        (Just (Point _ _ 'M'), Just (Point _ _ 'A'), Just (Point _ _ 'S')) -> True
        _ -> False

isxmastopright (Point x y _) grid =
  let m = findpoint (x + 1) (y - 1) grid
      a = findpoint (x + 2) (y - 2) grid
      s = findpoint (x + 3) (y - 3) grid
   in case (m, a, s) of
        (Just (Point _ _ 'M'), Just (Point _ _ 'A'), Just (Point _ _ 'S')) -> True
        _ -> False

isxmasbottomleft (Point x y _) grid =
  let m = findpoint (x - 1) (y + 1) grid
      a = findpoint (x - 2) (y + 2) grid
      s = findpoint (x - 3) (y + 3) grid
   in case (m, a, s) of
        (Just (Point _ _ 'M'), Just (Point _ _ 'A'), Just (Point _ _ 'S')) -> True
        _ -> False

isxmasbottomright (Point x y _) grid =
  let m = findpoint (x + 1) (y + 1) grid
      a = findpoint (x + 2) (y + 2) grid
      s = findpoint (x + 3) (y + 3) grid
   in case (m, a, s) of
        (Just (Point _ _ 'M'), Just (Point _ _ 'A'), Just (Point _ _ 'S')) -> True
        _ -> False

countnearbyxmas point grid =
  let t = if isxmastop point grid then 1 else 0
      r = if isxmasright point grid then 1 else 0
      b = if isxmasbottom point grid then 1 else 0
      l = if isxmasleft point grid then 1 else 0
      tl = if isxmastopleft point grid then 1 else 0
      tr = if isxmastopright point grid then 1 else 0
      bl = if isxmasbottomleft point grid then 1 else 0
      br = if isxmasbottomright point grid then 1 else 0
   in t + r + b + l + tl + tr + bl + br

countxmas grid = foldl' f 0 grid
  where
    f total point@(Point _ _ 'X') = total + countnearbyxmas point grid
    f total _ = total

getadjacents x y grid =
  let tl = findpoint (x - 1) (y - 1) grid
      tr = findpoint (x + 1) (y - 1) grid
      bl = findpoint (x - 1) (y + 1) grid
      br = findpoint (x + 1) (y + 1) grid
   in case (tl, tr, bl, br) of
        (Just (Point _ _ a), Just (Point _ _ b), Just (Point _ _ c), Just (Point _ _ d)) -> [a, b, c, d]
        _ -> ""

isvalidxmas (Point a b 'A') grid = getadjacents a b grid `elem` ["MSMS", "SMSM", "SSMM", "MMSS"]
isvalidxmas _ _ = False

countxmas2 grid = foldl' f 0 grid
  where
    f total point@(Point _ _ 'A') = total + (if isvalidxmas point grid then 1 else 0)
    f total _ = total

getgridfrominput = readFile "app/day4input.txt" >>= pure . inputtogrid

part1 = do
  grid <- getgridfrominput
  return $ countxmas grid :: IO Int

part2 = do
  grid <- getgridfrominput
  return $ countxmas2 grid :: IO Int