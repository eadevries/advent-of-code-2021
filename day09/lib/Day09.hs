module Day09 where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

import Util (readDataLines)

day09Part1 :: String -> String
day09Part1 s = case readDataLines linesToMaybeInts s of
    Left e   -> e
    Right xs -> show $ sum $ riskLevels $ toHeightMap xs

linesToMaybeInts :: String -> Maybe [Int]
linesToMaybeInts = mapM (readMaybe . pure)

toHeightMap :: [[Int]] -> M.Map (Int, Int) Int
toHeightMap xs = foldr (\(j, ns) m -> insertRow m j ns) M.empty (zip [0..] xs)
    where
        insertRow :: M.Map (Int, Int) Int -> Int -> [Int] -> M.Map (Int, Int) Int
        insertRow m j ns = foldr (\(i, n) -> M.insert (i, j) n) m (zip [0..] ns)

above :: (Int, Int) -> (Int, Int)
above (x, y) = (x, y - 1)

below :: (Int, Int) -> (Int, Int)
below (x, y) = (x, y + 1)

left :: (Int, Int) -> (Int, Int)
left (x, y) = (x - 1, y)

right :: (Int, Int) -> (Int, Int)
right (x, y) = (x + 1, y)

lowPoints :: M.Map (Int, Int) Int  -> [(Int, Int)]
lowPoints m = filter isLowPoint (M.keys m)
    where
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

-- The problem seems to presuppose that no two basins overlap; if they did we would also need to remove duplicates
day09Part2 :: String -> String
day09Part2 s = case readDataLines linesToMaybeInts s of
    Left e   -> e
    Right xs -> show $ product $ take 3 . reverse . sort . map (length . S.toList) $ calculateBasins $ toHeightMap xs

calculateBasins :: M.Map (Int, Int) Int -> [S.Set (Int, Int)]
calculateBasins m = map (\p -> addNeighborsToBasin m (S.singleton p) p) (lowPoints m)

addNeighborsToBasin :: M.Map (Int, Int) Int -> S.Set (Int, Int) -> (Int, Int) -> S.Set (Int, Int)
addNeighborsToBasin m s p = addNeighbor p (above p) $ addNeighbor p (below p) $ addNeighbor p (left p) $ addNeighbor p (right p) s
    where
        isInBasin :: (Int, Int) -> (Int, Int) -> Bool
        isInBasin pOld pNew = case m M.!? pNew of
            Just v -> v > m M.! pOld && v < 9
            Nothing -> False

        addNeighbor :: (Int, Int) -> (Int, Int) -> S.Set (Int, Int) -> S.Set (Int, Int)
        addNeighbor pOld pNew s' = 
            if S.member pNew s' || not (isInBasin pOld pNew)
            then s'
            else addNeighbor pNew (above pNew) $ addNeighbor pNew (below pNew) $ 
                    addNeighbor pNew (left pNew) $ addNeighbor pNew (right pNew) (S.insert pNew s')
