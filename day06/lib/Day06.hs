module Day06 where

import Text.Read (readMaybe)

import Util (readCSVLine)   

day06Part1 :: String -> String
day06Part1 s = case readCSVLine readFish s of
    Left e  -> e
    Right x -> show $ length $ last $ take 81 $ iterate fishTick x -- take n + 1 insetad of n, since the list starts with 0 applications

readFish :: String -> Maybe Int
readFish = readMaybe

-- (ha)
fishTick :: [Int] -> [Int]
fishTick fishList = fishTick' fishList [] []
    where
        fishTick' :: [Int] -> [Int] -> [Int] -> [Int]
        fishTick' []     updated newFish = reverse updated <> newFish -- reversing once at the end instead of concat-ing to the end of list every call
        fishTick' (0:fs) updated newFish = fishTick' fs (6:updated) (8:newFish)
        fishTick' (f:fs) updated newFish = fishTick' fs ((f - 1):updated) newFish
        
day06Part2 :: String -> String
day06Part2 s = undefined
