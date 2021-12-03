module Day02 where

import Text.Read ( readMaybe )
import Util (readDataLines)
import Data.Foldable (foldl')


data Move = Horizontal Int | Depth Int deriving Show

data Position = Position { hPos :: Int, depth :: Int, aim :: Int } deriving Show

day02Part1 :: String -> String
day02Part1 s = case readDataLines readMove s of
    Right xs -> show . productOfCoords . reckon $ xs
    Left e   -> e

day02Part2 :: String -> String
day02Part2 s = case readDataLines readMove s of
    Right xs -> show . productOfCoords . reckon' $ xs
    Left e   -> e

initialPosition :: Position
initialPosition = Position { hPos = 0, depth = 0, aim = 0 }

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

makeMove' :: Position -> Move -> Position
makeMove' p@(Position { hPos = h, depth = d, aim = a })  (Horizontal m) = p { hPos = h + m, depth = d + a * m }
makeMove' p@(Position { aim = a })                       (Depth m)      = p { aim = a + m }

reckon :: [Move] -> Position
reckon = foldl' makeMove initialPosition

reckon' :: [Move] -> Position
reckon' = foldl' makeMove' initialPosition

productOfCoords :: Position -> Int
productOfCoords (Position { hPos = h, depth = d }) = h * d