module Main where

import Day05 (day05Part1, day05Part2)
import Util (runPart, runDay)

inputFile :: FilePath
inputFile = "./day05/data/input.txt"

main :: IO ()
main = runDay inputFile day05Part1 day05Part2
