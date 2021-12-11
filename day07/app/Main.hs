module Main where

import Day07 (day07Part1, day07Part2)
import Util (runDay, runPart)

inputFile :: FilePath
inputFile = "./day07/data/input.txt"

main :: IO ()
main = runDay inputFile day07Part1 day07Part2
