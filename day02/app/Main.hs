module Main where

import Day02 ( day02Part1, day02Part2 )


inputFile :: FilePath
inputFile = "./day02/data/input.txt"

main :: IO ()
main = do
    fileStr <- readFile inputFile
    putStrLn $ "Part 1: " <> day02Part1 fileStr
    putStrLn $ "Part 2: " <> day02Part2 fileStr
