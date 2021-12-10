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
        
-- The above method is much too inefficient for larger numbers, as the number
-- of elements grows exponentially with the generations. For part two, instead
-- of adding elements and summing at the end, we just keep track of the sums at
-- each age.        
day06Part2 :: String -> String
day06Part2 s = case readCSVLine readFish s of
    Left e  -> e
    Right x -> show . sumFish . callNTimes 256 fishTick' . countFishAges $ x

countFishAges :: [Int] -> (Int,Int,Int,Int,Int,Int,Int,Int,Int)
countFishAges fs = 
    (
        countNs 0 fs,
        countNs 1 fs,
        countNs 2 fs,
        countNs 3 fs,
        countNs 4 fs,
        countNs 5 fs,
        countNs 6 fs,
        countNs 7 fs,
        countNs 8 fs
    )
    where
        countNs :: Int -> [Int] -> Int
        countNs n = length . filter (== n)

fishTick' :: (Int,Int,Int,Int,Int,Int,Int,Int,Int) -> (Int,Int,Int,Int,Int,Int,Int,Int,Int)
fishTick' (n0, n1, n2, n3, n4, n5, n6, n7, n8) = (n1, n2, n3, n4, n5, n6, n7 + n0, n8, n0)

sumFish :: (Int,Int,Int,Int,Int,Int,Int,Int,Int) -> Int
sumFish (n0, n1, n2, n3, n4, n5, n6, n7, n8) = n0 + n1 + n2 + n3 + n4 + n5 + n6 + n7 + n8

callNTimes :: Int -> (a -> a) -> a -> a
callNTimes n f a0 
    | n <= 0    = a0
    | otherwise = callNTimes (n - 1) f (f a0)