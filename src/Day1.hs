module Day1 (day1) where

import GHC.List (uncons)

parseInput :: IO [Int]
parseInput = fmap read . lines <$> readFile "inputs/1"

part1 :: [Int] -> Maybe Int
part1 ints = do
  (match, _) <- uncons [(x, y) | x <- ints, y <- ints, x + y == 2020]
  pure $ uncurry (*) match
  
part2 :: [Int] -> Maybe Int
part2 ints = do
  ((x', y', z'), _) <- uncons [(x, y, z) | x <- ints, y <- ints, z <- ints, x + y + z == 2020]
  pure $ x' * y' * z'

day1 :: IO String
day1 = do
  input <- parseInput
  pure $ "Part 1: " ++ show (part1 input) ++ "\nPart 2: " ++ show (part2 input) 
