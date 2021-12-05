module Main where

import Day03 (day03Part1, day03Part2)
import Util (runPart, runDay)

inputFile :: FilePath
inputFile = "./day03/data/input.txt"

main :: IO ()
main = runDay inputFile day03Part1 day03Part2
