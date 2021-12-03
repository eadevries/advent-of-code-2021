module Day02 where

import Text.Read ( readMaybe )
import Util (readDataLines)
import Data.Foldable (foldl')


data Move = Horizontal Int | Depth Int deriving Show

data Position = Position { hPos :: Int, depth :: Int } deriving Show

initialPosition :: Position
initialPosition = Position { hPos = 0, depth = 0 }

day02Part1 :: String -> String
day02Part1 s = case readDataLines readMove s of
    Right xs -> show . productOfCoords . reckon $ xs
    Left e   -> e

readDirection :: String -> Maybe (Int -> Move)
readDirection "forward"  = Just Horizontal
readDirection "backward" = Just (Horizontal . negate) -- Not part of specification, but added for completeness
readDirection "up"       = Just (Depth . negate)
readDirection "down"     = Just Depth
readDirection _          = Nothing

readMove :: String -> Maybe Move
readMove s = case words s of
    [dirStr, distStr] -> readDirection dirStr <*> readMaybe distStr
    _                 -> Nothing

makeMove :: Position -> Move -> Position
makeMove p@(Position { hPos = h })  (Horizontal m) = p { hPos = h + m }
makeMove p@(Position { depth = d }) (Depth m)      = p { depth = d + m }

reckon :: [Move] -> Position
reckon = foldl' makeMove initialPosition

productOfCoords :: Position -> Int
productOfCoords (Position { hPos = h, depth = d }) = h * d

data PositionAndAim = PositionAndAim { hPos' :: Int, depth' :: Int, aim' :: Int } deriving Show

initialPositionAndAim :: PositionAndAim
initialPositionAndAim = PositionAndAim { hPos' = 0, depth' = 0, aim' = 0 }

day02Part2 :: String -> String
day02Part2 s = case readDataLines readMove s of
    Right xs -> show . productOfCoords' . reckon' $ xs
    Left e   -> e

makeMove' :: PositionAndAim -> Move -> PositionAndAim
makeMove' p@(PositionAndAim { hPos' = h, depth' = d, aim' = a })  (Horizontal m) = p { hPos' = h + m, depth' = d + a * m }
makeMove' p@(PositionAndAim { aim' = a })                         (Depth m)      = p { aim' = a + m }

reckon' :: [Move] -> PositionAndAim
reckon' = foldl' makeMove' initialPositionAndAim

productOfCoords' :: PositionAndAim -> Int
productOfCoords' (PositionAndAim { hPos' = h, depth' = d }) = h * d