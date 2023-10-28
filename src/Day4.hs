module Day4 (day4) where

import Text.Parsec hiding (uncons)
import Data.List.Split (splitOn)
import qualified Data.Map.Lazy as Map
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
      pairs <- sepBy1 pair (many $ oneOf " \n")
      if length pairs == 
        (case filter ((== "cid") . fst) pairs of
          _ : _ -> 8
          [] -> 7
        )
      then pure ()
      else fail "Incorrect pair count"
    pair = byr <|> iyr <|> eyr <|> hgt <|> hcl <|> ecl <|> pid <|> cid
    byr = do
      k <- string' "byr"
      _ <- char ':'
      v <- (\year -> let year' = read year :: Int in if year' >= 1920 && year' <= 2002 then pure year else fail "Birth out of range") =<< count 4 digit
      pure (k, v)
    iyr = do
      k <- string' "iyr"
      _ <- char ':'
      v <- (\year -> let year' = read year :: Int in if year' >= 2010 && year' <= 2020 then pure year else fail "Issue out of range") =<< count 4 digit
      pure (k, v)
    eyr = do
      k <- string' "eyr"
      _ <- char ':'
      v <- (\year -> let year' = read year :: Int in if year' >= 2020 && year' <= 2030 then pure year else fail "Exp out of range") =<< count 4 digit
      pure (k, v)
    hgt = do
      k <- string' "hgt"
      _ <- char ':'
      num <- many1 digit
      let num' = read num :: Int
      m <- string "cm" <|> string "in"
      if (m == "cm" && (num' >= 150 && num' <= 193)) || (m == "in" && (num' >= 59 && num' <= 76))
      then pure (k, num ++ m)
      else fail "Height out of range"
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
      v <- many1 (noneOf " \n")
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
