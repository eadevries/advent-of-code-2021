module Main where

import Day06 (day06Part1, day06Part2)
import Util (runDay, runPart)

inputFile :: FilePath
inputFile = "./day06/data/input.txt"

main :: IO ()
main = runPart inputFile day06Part1
