module Day03 where

import qualified Data.Map.Strict as M
import Text.Read (readMaybe)

import Util (readDataLines)


day03Part1 :: String -> String
day03Part1 s = case readDataLines readSensorReading s of
    Right xs -> show $ gammaRate xs * epsilonRate xs
    Left e   -> e

readSensorReading :: String -> Maybe [Int]
readSensorReading = mapM readDigit
    where
        readDigit :: Char -> Maybe Int
        readDigit c = case readMaybe [c] of
            Just 0 -> Just 0
            Just 1 -> Just 1
            _      -> Nothing

readings :: [[Int]] -> M.Map Int Int
readings = foldr insertReading M.empty
    where
        withIndices :: [Int] -> [(Int, Int)]
        withIndices r = zip [length r - 1, length r - 2..0] r

        insertReading :: [Int] -> M.Map Int Int -> M.Map Int Int
        insertReading r m = foldr (\(i, d) acc -> M.insertWith (+) i d acc) m (withIndices r)

readingsMapToBinaryMap :: M.Map Int Int -> M.Map Int Int
readingsMapToBinaryMap = fmap (\n -> if n * 2 > 12 then 1 else 0)

binaryMapToInt :: M.Map Int Int -> Int
binaryMapToInt = M.foldrWithKey (\i  n total -> n * 2^i + total) 0

readingsMapToRate :: (Int -> Int) -> M.Map Int Int -> Int
readingsMapToRate f = binaryMapToInt . fmap f

-- 1's will be most common if their count is more than half the length 
-- of the readings list. Otherwise 0's will be more common.
gammaFunc :: Int -> Int -> Int
gammaFunc l n = if n * 2 >= l then 1 else 0

gammaRate :: [[Int]] -> Int
gammaRate r = readingsMapToRate (gammaFunc $ length r) . readings $ r

-- Same rationale as for gammaRate, but reversed.
epsilonFunc :: Int -> Int -> Int
epsilonFunc l n = if n * 2 < l then 1 else 0

epsilonRate :: [[Int]] -> Int
epsilonRate r = readingsMapToRate (epsilonFunc $ length r) . readings $ r

day03Part2 :: String -> String
day03Part2 s = case readDataLines readSensorReading s of
    Right xs -> case (*) <$> oxygenGenRating xs <*> co2ScrubberRating xs of
            Right n -> show n
            Left  e -> e
    Left e   -> e

filterReadings :: (Int -> Int -> Int) -> [[Int]] -> Either String [Int]
filterReadings f reads = filterReadings' (nextMatch reads) reads []
    where
        nextMatch :: [[Int]] -> Int
        nextMatch rs = f (length rs) $ foldr (\r total -> head r + total) 0 rs

        filterReadings' :: Int -> [[Int]] -> [Int] -> Either String [Int]
        filterReadings' fd rs ds = case filter (\r -> head r == fd) rs of
            []     -> Left "No matching readings"
            [x]    -> Right $ ds <> x
            xs     -> filterReadings' (nextMatch $ map tail xs) (map tail xs) (ds <> [fd])

rating :: (Int -> Int -> Int) -> [[Int]] -> Either String Int
rating f rs = binaryMapToInt . M.fromList . zip [0..] . reverse <$> filterReadings f rs
    where
        rm :: M.Map Int Int
        rm = readings rs

oxygenGenRating :: [[Int]] -> Either String Int
oxygenGenRating = rating gammaFunc

co2ScrubberRating :: [[Int]] -> Either String Int
co2ScrubberRating = rating epsilonFunc