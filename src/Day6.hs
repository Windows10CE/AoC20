module Day6 (day6) where

import Data.List.Split (splitOn)
import qualified Data.Set as S

parseGroup :: String -> [S.Set Char]
parseGroup input = S.fromList <$> lines input

parseInput :: IO [[S.Set Char]]
parseInput = fmap parseGroup . splitOn "\n\n" <$> readFile "inputs/6"

part1 :: [[S.Set Char]] -> Int
part1 groups = sum $ length . foldr S.union S.empty <$> groups

part2 :: [[S.Set Char]] -> Int
part2 groups = sum $ length . foldr S.intersection (S.fromList ['a'..'z']) <$> groups

day6 :: IO String
day6 = do
  input <- parseInput
  pure $ "Part 1: " ++ show (part1 input) ++ "\nPart 2: " ++ show (part2 input)
