module Day10 (day10) where

import Data.List (sort)

parseInput :: IO [Int]
parseInput = sort . fmap read . lines <$> readFile "inputs/10"

part1 :: [Int] -> (Int, Int)
part1 jolts =
  let (_, one, three) = foldl handleJolt (0, 0, 0) jolts
  in (one, three)
  where
    handleJolt (current, one, three) next = error ""

day10 :: IO String
day10 = pure ""
