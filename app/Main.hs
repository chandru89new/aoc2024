module Main where

import Control.Monad (void)
import qualified Day1 as D1
import qualified Day2 as D2
import qualified Day3 as D3
import qualified Day4 as D4
import qualified Day5 as D5
import qualified Day6 as D6
import qualified Day7 as D7
import qualified Day8 as D8
import System.Environment (getArgs)
import System.IO

main :: IO ()
main = do
  _ <- putStr "Day? "
  hFlush stdout
  l <- getLine
  day <- pure $ read l :: IO Int
  case day of
    1 -> do
      p1 <- D1.part1
      p2 <- D1.part2
      showResults p1 p2
    2 -> do
      p1 <- D2.part1
      p2 <- D2.part2
      showResults p1 p2
    3 -> do
      p1 <- D3.part1
      p2 <- D3.part2
      showResults p1 p2
    4 -> do
      p1 <- D4.part1
      p2 <- D4.part2
      showResults p1 p2
    5 -> do
      p1 <- D5.part1
      p2 <- D5.part2
      showResults p1 p2
    6 -> do
      p1 <- D6.part1
      p2 <- D6.part2
      showResults p1 p2
    7 -> do
      p1 <- D7.part1
      p2 <- D7.part2
      showResults p1 p2
    8 -> do
      p1 <- D8.part1
      p2 <- D8.part2
      showResults p1 p2
    _ -> putStrLn "Invalid day"

showResults p1 p2 = do
  putStrLn $ "part1: " ++ show p1
  putStrLn $ "part2: " ++ show p2