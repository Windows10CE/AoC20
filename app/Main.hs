module Main (main) where

import System.Environment (getArgs)
import Day1
import Day2
import Day3

main :: IO ()
main = do
  args <- getArgs
  res <- case args of
    ["1"] -> day1
    ["2"] -> day2
    ["3"] -> day3
    _ -> error "no day"
  putStrLn res
