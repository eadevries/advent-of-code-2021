module Day07 where

import Data.List (elemIndex)
import Text.Read (readMaybe)

import Util (readCSVLine)


day07Part1 :: String -> String
day07Part1 s = case readCSVLine (readMaybe :: String -> Maybe Int) s of
    Left e   -> e
    Right [] -> "Empty list"
    Right x  -> show $ minSum x

sumOfDistances :: [Int] -> Int -> Int
sumOfDistances ns x = sum $ [abs . (x - )] <*> ns

distanceSumList :: [Int] -> [Int]
distanceSumList ns = map (sumOfDistances ns) [minimum ns .. maximum ns]

minSum :: [Int] -> Int
minSum = minimum . distanceSumList

day07Part2 :: String -> String 
day07Part2 s = case readCSVLine (readMaybe :: String -> Maybe Int) s of
    Left e   -> e
    Right [] -> "Empty list"
    Right x  -> show $ minSum' x

sumOfDistancesWith :: (Int -> Int -> Int) -> [Int] -> Int -> Int
sumOfDistancesWith f ns x = sum $ [f x] <*> ns

-- The sum of the numbers from 1 to n is n * (n + 1) / 2. 
fuelConsumption :: Int -> Int -> Int
fuelConsumption start finish = (dist + 1) * dist `div` 2
    where
        dist :: Int
        dist = abs (finish - start)

distanceSumList' :: [Int] -> [Int]
distanceSumList' ns = map (sumOfDistancesWith fuelConsumption ns) [minimum ns .. maximum ns]

minSum' :: [Int] -> Int
minSum' = minimum . distanceSumList'

