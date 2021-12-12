module Day09 where

import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

import Util (readDataLines)

day09Part1 :: String -> String
day09Part1 s = case readDataLines lineToMaybeInts s of
    Left e   -> e
    Right xs -> show $ sum $ riskLevels $ toHeightMap xs

lineToMaybeInts :: String -> Maybe [Int]
lineToMaybeInts = mapM (readMaybe . pure)

toHeightMap :: [[Int]] -> M.Map (Int, Int) Int
toHeightMap xs = foldr (\(j, ns) m -> insertRow m j ns) M.empty (zip [0..] xs)
    where
        insertRow :: M.Map (Int, Int) Int -> Int -> [Int] -> M.Map (Int, Int) Int
        insertRow m j ns = foldr (\(i, n) -> M.insert (i, j) n) m (zip [0..] ns)

lowPoints :: M.Map (Int, Int) Int  -> [(Int, Int)]
lowPoints m = filter isLowPoint (M.keys m)
    where
        above :: (Int, Int) -> (Int, Int)
        above (x, y) = (x, y - 1)

        below :: (Int, Int) -> (Int, Int)
        below (x, y) = (x, y + 1)

        left :: (Int, Int) -> (Int, Int)
        left (x, y) = (x - 1, y)

        right :: (Int, Int) -> (Int, Int)
        right (x, y) = (x + 1, y)

        getNeighbors :: (Int, Int) -> [(Int, Int)]
        getNeighbors p = foldr (\f xs -> case M.lookup (f p) m of
                Just _ -> f p:xs
                Nothing -> xs
            ) [] [above, below, left, right]

        isLowPoint :: (Int, Int) -> Bool
        isLowPoint p = all (\x -> case (<) <$> M.lookup p m <*> M.lookup x m of
                Just True -> True
                _         -> False
            ) (getNeighbors p)

riskLevels :: M.Map (Int, Int) Int -> [Int]
riskLevels m = map (\v -> 1 + m M.! v) (lowPoints m)

day09Part2 :: String -> String
day09Part2 s = undefined