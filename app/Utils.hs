module Utils where

foldInput :: a -> (Int -> Int -> Char -> a -> a) -> String -> a
foldInput initValue updateFn str = go 0 initValue $ lines str
  where
    -- go :: Int -> a -> [String] -> a
    go _ acc [] = acc
    go lineIdx acc (line : rest) = go (lineIdx + 1) (foldLine acc (updateFn lineIdx) line) rest

    foldLine :: a -> (Int -> Char -> a -> a) -> String -> a
    foldLine accumValue fn line = go' 0 line accumValue
      where
        go' _ [] acc = acc
        go' itemIdx (c : rest) acc = go' (itemIdx + 1) rest (fn itemIdx c acc)