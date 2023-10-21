module Day2 (day2) where

import Text.Parsec hiding (letter)
import Data.Either (fromRight)

data Entry = Entry { low :: Int, high:: Int, letter :: Char, password :: String } deriving (Show)

parseEntry :: String -> Entry
parseEntry s =
  fromRight (error "failed to parse") $ parse parser "" s
  where
    parser = do
      low' <- many1 digit
      _ <- char '-'
      high' <- many1 digit
      _ <- space
      letter' <- lower
      _ <- string ": "
      password' <- many1 lower
      pure $ Entry (read low') (read high') letter' password'

parseInput :: IO [Entry]
parseInput = fmap parseEntry . lines <$> readFile "inputs/2"

part1 :: [Entry] -> Int
part1 entries = length $ filter valid entries
  where
    valid e = let num = length $ filter (== letter e) (password e)
              in num >= low e && num <= high e

part2 :: [Entry] -> Int
part2 entries = length $ filter valid entries
  where
    valid e = ((password e !! (low e - 1)) == letter e) `xor` ((password e !! (high e - 1)) == letter e)
    xor a b = (a /= b) && (a || b)

day2 :: IO String
day2 = do
  input <- parseInput
  pure $ "Part 1: " ++ show (part1 input) ++ "\nPart 2: " ++ show (part2 input)
