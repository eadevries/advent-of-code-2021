module Main where

import Day04 (day04Part1, day04Part2)
import Util (runPart, runDay)

inputFile :: FilePath
inputFile = "./day04/data/input.txt"

main :: IO ()
main = runPart inputFile day04Part1
-- main = runDay inputFile day04Part1 day04Part2
