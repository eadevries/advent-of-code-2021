module Day04 where

import Data.List (delete)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe, readEither)

import Util (headMaybe)

data Board = 
    Board { 
        emptySquares :: [Int]
      , rows :: [[Int]]
      , cols :: [[Int]]
    } deriving Show

board :: [[Int]] -> Board
board b = Board { emptySquares = es, rows = b, cols = cs [] b }
    where
        es :: [Int]
        es = concat b

        cs :: [[Int]] -> [[Int]] -> [[Int]]
        cs c ([]:rs) = c
        cs c r       = cs (c <> [map head r]) (map tail r)

day04Part1 :: String -> String
day04Part1 s = case readDrawListAndBoards s of
    Left e         -> e
    Right (dl, bs) -> maybe "No winner found" (show . uncurry winningScore) (winningBoard dl bs)

readDrawListAndBoards :: String -> Either String ([Int], [Board])
readDrawListAndBoards s = (,) <$> readDrawList s <*> readBoards s

readDrawList :: String -> Either String [Int]
readDrawList s = firstLineEither >>= mapM readEither . splitOn ","
    where
        firstLineEither :: Either String String
        firstLineEither = case headMaybe . lines $ s of
            Nothing -> Left "Unable to extract first line for draw list"
            Just x  -> Right x

readBoards :: String -> Either String [Board]
readBoards = mapM readBoard . tail . splitOn "\n\n"

readBoard :: String -> Either String Board
readBoard boardStr = case lines boardStr of
    b@[r1, r2, r3, r4, r5] -> board <$> mapM readBoardRow b
    _                      -> Left $ "Error parsing board from:\n" <> boardStr

readBoardRow :: String -> Either String [Int]
readBoardRow rowStr = case words rowStr of
    r@[e1, e2, e3, e4, e5] -> mapM readEither r
    _                      -> Left $ "Error parsing row:\n" <> rowStr

removeDraw :: Int -> Board -> Board
removeDraw d b = b { emptySquares = es, rows = rs, cols = cs }
    where
        es :: [Int]
        es = delete d $ emptySquares b

        rs :: [[Int]]
        rs = map (delete d) (rows b)

        cs :: [[Int]]
        cs = map (delete d) (cols b)

hasBingo :: Board -> Bool
hasBingo b = any null (rows b) || any null (cols b)

winningBoard :: [Int] -> [Board] -> Maybe (Int, Board)
winningBoard []           boards = Nothing
winningBoard (draw:draws) boards = case winner of 
    Just w  -> Just (draw, w)
    Nothing -> winningBoard draws updatedBoards
    where
        updatedBoards :: [Board]
        updatedBoards = map (removeDraw draw) boards

        winner :: Maybe Board
        winner = case dropWhile (not . hasBingo) updatedBoards of
            []  -> Nothing
            b:_ -> Just b

winningScore :: Int -> Board -> Int
winningScore d b = d * (sum . map sum $ rows b)

lastWinningBoard :: [Int] -> [Board] -> Maybe (Int, Board)
lastWinningBoard []    boards = Nothing
lastWinningBoard draws boards = lastWinningBoard' draws boards
    where
        updateBoards :: Int -> [Board] -> [Board]
        updateBoards n = map (removeDraw n)

        lastWinningBoard' :: [Int] -> [Board] -> Maybe (Int, Board)
        lastWinningBoard' ns bs = case filter (not . hasBingo) $ updateBoards (head ns) bs of
            []  -> Nothing
            [b] -> winningBoard (tail ns) [b]
            bs' -> lastWinningBoard' (tail ns) bs'

day04Part2 :: String -> String
day04Part2 s =  case readDrawListAndBoards s of
    Left e         -> e
    Right (dl, bs) -> maybe "No winner found" (show . uncurry winningScore) (lastWinningBoard dl bs)
