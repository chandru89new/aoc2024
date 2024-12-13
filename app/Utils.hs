{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Utils where

fold_input :: a -> (Int -> Int -> Char -> a -> a) -> String -> a
fold_input init_value update_fn str = go 0 init_value $ lines str
  where
    -- go :: Int -> a -> [String] -> a
    go _ acc [] = acc
    go line_idx acc (line : rest) = go (line_idx + 1) (fold_line acc (update_fn line_idx) line) rest

    fold_line :: a -> (Int -> Char -> a -> a) -> String -> a
    fold_line accum_value fn line = go' 0 line accum_value
      where
        go' _ [] acc = acc
        go' item_idx (c : rest) acc = go' (item_idx + 1) rest (fn item_idx c acc)