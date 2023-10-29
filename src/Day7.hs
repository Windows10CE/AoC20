module Day7 (day7) where

import Data.Functor (($>))
import Text.Parsec
import Data.Either (fromRight)
import Data.Maybe (fromJust)
import Data.Foldable (find)
import qualified Data.Set as S

parseBag :: String -> (String, [(String, Int)])
parseBag input = fromRight (error "") $ parse parser "" input
  where
    parser = do
      name <- bagName
      _ <- string " contain "
      containsList <- (try (string "no other bags.") $> []) <|> sepBy1 containedBag (string ", ")
      pure (name, containsList)
    bagName = do
      adjective <- many1 letter
      _ <- space
      color <- many1 letter
      _ <- string " bag"
      _ <- optional $ char 's'
      pure $ adjective ++ " " ++ color
    containedBag = do
      number <- many1 digit
      _ <- space
      name <- bagName
      pure (name, read number)

parseInput :: IO [(String, [(String, Int)])]
parseInput = fmap parseBag . lines <$> readFile "inputs/7"

recursivePart1 :: [(String, [(String, Int)])] -> S.Set String -> S.Set String
recursivePart1 bags oldSet =
  let newSet = S.union oldSet $ S.fromList $ fst <$> filter (any ((`S.member` oldSet) . fst) . snd) bags
  in if length newSet /= length oldSet then recursivePart1 bags newSet else newSet

part1 :: [(String, [(String, Int)])] -> Int
part1 bags = length $ S.delete "shiny gold" $ recursivePart1 bags (S.singleton "shiny gold")

findBagCost :: [(String, [(String, Int)])] -> String -> Int
findBagCost bags name = sum $ getInnerCosts $ snd $ fromJust $ find ((== name) . fst) bags
  where
    getInnerCosts contained =
      (\(name', count') -> count' + (findBagCost bags name' * count')) <$> contained

part2 :: [(String, [(String, Int)])] -> Int
part2 bags = findBagCost bags "shiny gold"

day7 :: IO String
day7 = do
  input <- parseInput
  pure $ "Part 1: " ++ show (part1 input) ++ "\nPart 2: " ++ show (part2 input)
