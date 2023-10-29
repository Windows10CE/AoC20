module Day8 (day8) where

import Text.Parsec
import Data.List (find)
import Data.Maybe (fromJust)
import qualified Data.Set as S

data Instruction =
  Acc Int
  | Jmp Int
  | Nop Int

(!?) :: [a] -> Int -> Maybe a
xs !? i = if i < 0 || i >= length xs then Nothing else Just $ xs !! i

replace :: [a] -> Int -> a -> [a]
replace xs i x =
  case splitAt i xs of
    (prefix, _:rest) -> prefix ++ (x : rest)
    _ -> error "index out of range"

parseInstructions :: String -> [Instruction]
parseInstructions input = case parse parser "" input of
    Left e -> error $ show e
    Right v -> v
  where
    parser = many instruction
    instruction = do
      opcode <- string "acc" <|> string "jmp" <|> string "nop"
      _ <- space
      operand <- (char '+' *> many1 digit) <|> (char '-' *> (('-' :) <$> many1 digit))
      _ <- spaces
      case opcode of
        "acc" -> pure $ Acc $ read operand
        "jmp" -> pure $ Jmp $ read operand
        "nop" -> pure $ Nop $ read operand
        _ -> error "unreachable"

parseInput :: IO [Instruction]
parseInput = parseInstructions <$> readFile "inputs/8"

runInstruction :: [Instruction] -> S.Set Int -> Int -> Int -> (Int, Bool)
runInstruction insts seen current acc =
  if S.notMember current seen
  then case insts !? current of
    Just (Acc modifier) -> runInstruction insts (S.insert current seen) (current + 1) (acc + modifier)
    Just (Jmp modifier) -> runInstruction insts (S.insert current seen) (current + modifier) acc
    Just (Nop _) -> runInstruction insts (S.insert current seen) (current + 1) acc
    Nothing -> if current == length insts then (acc, True) else (acc, False)
  else (acc, False)

part1 :: [Instruction] -> Int
part1 insts = fst $ runInstruction insts S.empty 0 0

part2 :: [Instruction] -> Int
part2 insts = fst $ fromJust $ find snd $ correct <$> [0..(length insts - 1)]
  where
    correct index =
      case insts !! index of
        Acc _ -> (0, False)
        Jmp offset -> runInstruction (replace insts index (Nop offset)) S.empty 0 0
        Nop offset -> runInstruction (replace insts index (Jmp offset)) S.empty 0 0

day8 :: IO String
day8 = do
  input <- parseInput
  pure $ "Part 1: " ++ show (part1 input) ++ "\nPart 2: " ++ show (part2 input)
