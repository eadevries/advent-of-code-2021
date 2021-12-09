{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Day05 where

import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Text.Read (readMaybe)

import Util (readDataLines)


data VentLine = VentLine {
    x1 :: Int, y1 :: Int,
    x2 :: Int, y2 :: Int
} deriving Show

ventLine :: (Int,Int) -> (Int,Int) -> VentLine
ventLine (x,y) (x',y') = VentLine { x1 = x, y1 = y, x2 = x', y2 = y' }

isHorizontal :: VentLine -> Bool
isHorizontal VentLine { y1, y2 } = y1 == y2

isVertical :: VentLine -> Bool
isVertical VentLine { x1, x2 } = x1 == x2

ventLinePoints :: VentLine -> [(Int,Int)]
ventLinePoints v@(VentLine { x1, y1, x2, y2 })
    | isHorizontal v = map (,y1) [xMin .. xMax]
    | isVertical v   = map (x1,) [yMin .. yMax]
    | otherwise      = []
    where
        xMin :: Int
        xMin = min x1 x2

        yMin :: Int
        yMin = min y1 y2

        xMax :: Int
        xMax = max x1 x2

        yMax :: Int
        yMax = max y1 y2

ventLineMap :: [(Int,Int)] -> M.Map (Int,Int) Int
ventLineMap = M.fromListWith (+) . map (,1)

highDensityCount :: M.Map (Int,Int) Int -> Int
highDensityCount = foldr (\n acc -> if n > 1 then acc + 1 else acc) 0

day05Part1 :: String -> String
day05Part1 s = case readDataLines readVentLine s of
    Left e  -> e
    Right x -> show . highDensityCount . ventLineMap . concatMap ventLinePoints $ x

parseCoords :: String -> Maybe (Int,Int)
parseCoords c = case splitOn "," c of
    [x,y] -> (,) <$> readMaybe x <*> readMaybe y
    _     -> Nothing

readVentLine :: String -> Maybe VentLine
readVentLine s = case words s of
    [coords1, _, coords2] -> ventLine <$> parseCoords coords1 <*> parseCoords coords2
    _                     -> Nothing

ventLinePointsWithDiagonals :: VentLine -> [(Int, Int)]
ventLinePointsWithDiagonals v@(VentLine { x1, y1, x2, y2 })
    | isNotDiagonal   = ventLinePoints v
    | is45DegDiagonal = zip (range x1 x2) (range y1 y2)
    | otherwise       = []
    where
        range :: Int -> Int -> [Int]
        range a b = if a < b then [a .. b] else reverse [b .. a]

        isNotDiagonal :: Bool
        isNotDiagonal = isHorizontal v || isVertical v
        
        is45DegDiagonal :: Bool
        is45DegDiagonal = abs (x1 - x2) == abs (y1 - y2)

day05Part2 :: String -> String
day05Part2 s = case readDataLines readVentLine s of
    Left e  -> e
    Right x -> show . highDensityCount . ventLineMap . concatMap ventLinePointsWithDiagonals $ x
