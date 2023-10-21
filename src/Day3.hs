module Day3 (day3) where

(%!!) :: [a] -> Int -> a
xs %!! i = xs !! (i `mod` length xs)

parseInput :: IO [[Bool]]
parseInput = fmap ((== '#') <$>) . lines <$> readFile "inputs/3"

doSlope :: Int -> Int -> Int -> [[Bool]] -> [Bool]
doSlope across down current trees =
  case trees of
    lane : _ -> lane %!! current : doSlope across down (current + across) (drop down trees)
    [] -> []

forSlope :: Int -> Int -> [[Bool]] -> Int
forSlope across down trees =
  length $ filter id $ doSlope across down across (drop down trees)

part1 :: [[Bool]] -> Int
part1 = forSlope 3 1

part2 :: [[Bool]] -> Int
part2 trees =
  product $ (\f -> f trees) . uncurry forSlope <$> [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

day3 :: IO String
day3 = do
  input <- parseInput
  pure $ "Part 1: " ++ show (part1 input) ++ "\nPart 2: " ++ show (part2 input)
