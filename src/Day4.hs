module Day4 (day4) where

import Text.Parsec hiding (uncons)
import Data.List.Split (splitOn)
import Data.Char (isSpace)
import qualified Data.Map.Lazy as Map
import qualified Data.Set as S
import Debug.Trace (trace)

parsePassport1 :: String -> Map.Map String String
parsePassport1 input = case parse parser "" input of
  Left e -> error $ show e
  Right val -> val
  where
    parser = Map.fromList <$> many pair
    pair = do
      key <- many1 $ noneOf ":"
      _ <- char ':'
      value <- many1 $ noneOf " \n"
      _ <- spaces
      pure (key, value)

parsePassport2 :: String -> Bool
parsePassport2 input = case parse parser "" input of
  Left e -> trace (input ++ "\n" ++ show e ++ "\n\n") False
  Right _ -> True
  where
    parser = do
      pairs <- fmap fst <$> sepBy1 pair spaces
      let pairs' = "cid" : pairs -- jank to make sure cid is always there
      if S.fromList pairs' == S.fromList ["cid", "byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
      then pure ()
      else fail "Incorrect pair count"

pair = byr <|> iyr <|> eyr <|> hgt <|> hcl <|> ecl <|> pid <|> cid

yearBetween :: String -> Int -> Int -> Parsec String u (String, String)
yearBetween key start end = do
  k <- string' key
  _ <- char ':'
  year <- count 4 digit
  let year' = read year :: Int
  if year' >= start && year' <= end
  then pure (k, year)
  else fail "Out of range"
byr = yearBetween "byr" 1920 2002
iyr = yearBetween "iyr" 2010 2020
eyr = yearBetween "eyr" 2020 2030
hgt = do
  k <- string' "hgt"
  _ <- char ':'
  num <- many1 digit
  m <- string "cm" <|> string "in"
  let num' = read num :: Int
  case m of
    "cm" -> if num' >= 150 && num' <= 193
            then pure (k, num ++ m)
            else fail "Height out of range"
    "in" -> if num' >= 59 && num' <= 76
            then pure (k, num ++ m)
            else fail "Height out of range"
    _ -> fail "Invalid height unit"
hcl = do
  k <- string' "hcl"
  _ <- string ":#"
  v <- count 6 (oneOf "0123456789abcdef")
  pure (k, v)
ecl = do
  k <- string' "ecl"
  _ <- char ':'
  v <- string' "amb" <|> string' "blu" <|> string' "brn" <|> string' "gry" <|> string' "grn" <|> string' "hzl" <|> string' "oth"
  pure (k, v)
pid = do
  k <- string' "pid"
  _ <- char ':'
  v <- count 9 digit
  pure (k, v)
cid = do
  k <- string' "cid"
  _ <- char ':'
  v <- many (satisfy $ not . isSpace)
  pure (k, v)

parseInput :: (String -> a) -> IO [a]
parseInput parser = fmap parser . splitOn "\n\n" <$> readFile "inputs/4"

part1 :: [Map.Map String String] -> Int
part1 passports = length $ filter valid passports
  where
    valid passport =
      Map.size passport == case Map.lookup "cid" passport of
        Just _ -> 8
        Nothing -> 7

part2 :: [Bool] -> Int
part2 bs = length $ filter id bs

day4 :: IO String
day4 = do
  input1 <- parseInput parsePassport1
  input2 <- parseInput parsePassport2
  pure $ "Part 1: " ++ show (part1 input1) ++ "\nPart 2: " ++ show (part2 input2)
