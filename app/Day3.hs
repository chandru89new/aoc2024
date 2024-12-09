{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

module Day3 where

import Data.List (isInfixOf)
import Text.RawString.QQ
import Text.Regex.PCRE

getmatches teststring = case (teststring =~ [r|mul\((-?\d+),(-?\d+)\)|]) :: [[String]] of
  [] -> []
  xs -> map f xs
    where
      f (_ : a : b : _) = Just (read a :: Int, read b :: Int)
      f _ = Nothing

data Instruction = Do | Dont | Mul (Int, Int) | Invalid deriving (Show, Eq)

getinstructions str = case (str =~ [r|(mul\((-?\d+),(-?\d+)\)|(do\(\))|(don't\(\)))|]) :: [[String]] of
  [] -> []
  xs -> map f xs
    where
      f (instr : _ : a : b : _)
        | "mul" `isInfixOf` instr = Mul (read a :: Int, read b :: Int)
        | instr == "do()" = Do
        | instr == "don't()" = Dont
        | otherwise = Invalid
      f _ = Invalid

part1 = do
  input <- readFile "app/day3input.txt"
  return $ foldl f 0 (getinstructions input)
  where
    f acc (Mul (a, b)) = acc + a * b
    f acc _ = acc

part2 = do
  input <- readFile "app/day3input.txt"
  return $ fst $ foldl f (0, True) (getinstructions input)
  where
    f (acc, shouldcalc) (Mul (a, b)) = if shouldcalc then (acc + a * b, shouldcalc) else (acc, shouldcalc)
    f (acc, _) Dont = (acc, False)
    f (acc, _) Do = (acc, True)
    f (acc, s) _ = (acc, s)
