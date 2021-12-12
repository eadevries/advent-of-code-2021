module Main where

import Day09 (day09Part1, day09Part2)
import Util (runDay, runPart)

inputFile :: FilePath
inputFile = "./day09/data/input.txt"

main :: IO ()
main = runPart inputFile day09Part1
