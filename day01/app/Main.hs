module Main where

import Day01 (day01Part1, day01Part2)

main :: IO ()
main = do 
    input <- readFile "./data/input.txt"
    putStrLn $ "Part 1: " <> day01Part1 input
    putStrLn $ "Part 2: " <> day01Part2 input


