module Day9 (day9) where

import Data.Maybe (fromJust, isJust, isNothing)
import Data.List (find, uncons)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class (lift)
import Control.Monad (foldM)

parseInput :: IO [Int]
parseInput = fmap read . lines <$> readFile "inputs/9"

part1 :: [Int] -> Int
part1 nums = fst $ fromJust $ find (uncurry correct) $ drop 25 $ zip nums [0..]
  where
    correct num index =
      let nums' = take 25 $ drop (index - 25) nums
      in isNothing $ uncons [() | x <- nums', y <- nums', x + y == num]

part2 :: [Int] -> Int
part2 nums = (\(_, xs) -> minimum xs + maximum xs) $ fromJust $ fromJust $ find isJust $ handleOffset <$> [0..]
  where
    target = part1 nums
    handleOffset :: Int -> Maybe (Int, [Int])
    handleOffset off =
      let nums' = drop off nums
      in foldM handleAddition (0, []) (lift nums' :: MaybeT [] Int)
    handleAddition :: (Int, [Int]) -> Int -> Maybe (Int, [Int])
    handleAddition (currentSum, list) next
      | currentSum == target = Just (currentSum, list)
      | currentSum + next > target = Nothing
      | otherwise = Just (currentSum + next, next : list)

day9 :: IO String
day9 = do
  input <- parseInput
  pure $ "Part 1: " ++ show (part1 input) ++ "\nPart 2: " ++ show (part2 input)
