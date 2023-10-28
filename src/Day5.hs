module Day5 (day5) where

import Data.List (find)
import Data.Maybe (fromJust)
import qualified Data.Set as S

data Seat = Seat Int Int

parseSeat :: String -> Seat
parseSeat str =
  let (row, column) = foldl iteration ([0..127], [0..7]) str
  in Seat (head row) (head column)
  where
    iteration (rows, columns) c =
      case c of
        'F' -> (take (length rows `quot` 2) rows, columns)
        'B' -> (drop (length rows `quot` 2) rows, columns)
        'L' -> (rows, take (length columns `quot` 2) columns)
        'R' -> (rows, drop (length columns `quot` 2) columns)
        _ -> error "damn"

parseInput :: IO [Seat]
parseInput = fmap parseSeat . lines <$> readFile "inputs/5"

part1 :: [Seat] -> Int
part1 seats = maximum $ (\(Seat row column) -> row * 8 + column) <$> seats

part2 :: [Seat] -> Int
part2 seats =
  let set = S.fromList $ (\(Seat row column) -> row * 8 + column) <$> seats
  in fromJust $ find (\seat -> S.notMember seat set && S.member (seat - 1) set && S.member (seat + 1) set) [0..]

day5 :: IO String
day5 = do
  input <- parseInput
  pure $ "Part 1: " ++ show (part1 input) ++ "\nPart 2: " ++ show (part2 input)
