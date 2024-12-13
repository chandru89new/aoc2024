{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Day3 where

import Data.List (isInfixOf)
import Text.RawString.QQ
import Text.Regex.PCRE

get_matches test_string = case (test_string =~ [r|mul\((-?\d+),(-?\d+)\)|]) :: [[String]] of
  [] -> []
  xs -> map f xs
    where
      f (_ : a : b : _) = Just (read a :: Int, read b :: Int)
      f _ = Nothing

data Instruction = Do | Dont | Mul (Int, Int) | Invalid deriving (Show, Eq)

get_instructions str = case (str =~ [r|(mul\((-?\d+),(-?\d+)\)|(do\(\))|(don't\(\)))|]) :: [[String]] of
  [] -> []
  xs -> map f xs
    where
      f (instr : _ : a : b : _)
        | "mul" `isInfixOf` instr = Mul (read a :: Int, read b :: Int)
        | instr == "do()" = Do
        | instr == "don't()" = Dont
        | otherwise = Invalid
      f _ = Invalid

part_1 = do
  input <- readFile "app/day3input.txt"
  return $ foldl f 0 (get_instructions input)
  where
    f acc (Mul (a, b)) = acc + a * b
    f acc _ = acc

part_2 = do
  input <- readFile "app/day3input.txt"
  return $ fst $ foldl f (0, True) (get_instructions input)
  where
    f (acc, should_calc) (Mul (a, b)) = if should_calc then (acc + a * b, should_calc) else (acc, should_calc)
    f (acc, _) Dont = (acc, False)
    f (acc, _) Do = (acc, True)
    f (acc, s) _ = (acc, s)
