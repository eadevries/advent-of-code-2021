module Day01 where

import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

day01Part1 :: String -> String
day01Part1 s = case depths s of
    Right ds -> show . countIncreases . changes $ ds
    Left  e  -> e

depths :: String -> Either String [Int]
depths s = case maybeDepths s of
    Just ds -> Right ds
    Nothing -> Left "Error processing data"
    where
        maybeDepths :: String -> Maybe [Int]
        maybeDepths = mapM readMaybe . lines

data ChangeType = Increase | Decrease | Level deriving (Show, Eq)

changes :: [Int] -> [ChangeType]
changes s = zipWith zipper (tail s) s
    where
        zipper :: Int -> Int -> ChangeType
        zipper n2 n1 | n1 < n2 = Increase
                     | n1 > n2 = Decrease
                     | otherwise = Level

countIncreases :: [ChangeType] -> Int
countIncreases = sum . map (\ch -> if ch == Increase then 1 else 0)

day01Part2 :: String -> String 
day01Part2 s = case depths s of
    Right ds -> show . countIncreases . changes . averages $ ds
    Left  e  -> e

averages :: [Int] -> [Int]
averages s = zipWith3 (\a b c -> a + b + c) (drop 2 s) (tail s) s
