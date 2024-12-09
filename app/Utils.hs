module Utils where

foldinput :: a -> (Int -> Int -> Char -> a -> a) -> String -> a
foldinput initvalue updatefn str = go 0 initvalue $ lines str
  where
    -- go :: Int -> a -> [String] -> a
    go _ acc [] = acc
    go lineidx acc (line : rest) = go (lineidx + 1) (foldline acc (updatefn lineidx) line) rest

    foldline :: a -> (Int -> Char -> a -> a) -> String -> a
    foldline accumvalue fn line = go' 0 line accumvalue
      where
        go' _ [] acc = acc
        go' itemidx (c : rest) acc = go' (itemidx + 1) rest (fn itemidx c acc)