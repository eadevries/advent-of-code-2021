module Util where

import Data.List.Split (splitOn)

runDay :: FilePath -> (String -> String) -> (String -> String) -> IO ()
runDay inputFile part1 part2 = do
    fileStr <- readFile inputFile
    putStrLn $ "Part 1: " <> part1 fileStr
    putStrLn $ "Part 2: " <> part2 fileStr

runPart :: FilePath -> (String -> String) -> IO ()
runPart inputFile part = do
    fileStr <- readFile inputFile
    putStrLn $ "Result: " <> part fileStr

readDataLines :: (String -> Maybe a) -> String -> Either String [a]
readDataLines f s = case mapM f (lines s) of
    Just xs -> Right xs
    Nothing -> Left "Error parsing data"

readCSVLine :: (String -> Maybe a) -> String -> Either String [a]
readCSVLine f s = case mapM f (splitOn "," s) of
    Just xs -> Right xs
    Nothing -> Left "Error parsing CSV line"

readCSVLines :: (String -> Maybe a) -> String -> Either String [[a]]
readCSVLines f s = mapM (readCSVLine f) (lines s)

headMaybe :: [a] -> Maybe a
headMaybe []    = Nothing
headMaybe (x:_) = Just x