module Main where

import Util (runDay, runPart)
import Day08 (day08Part1, day08Part2)

inputFile :: FilePath
inputFile = "./day08/data/input.txt"

main :: IO ()
main = runDay inputFile day08Part1 day08Part2
