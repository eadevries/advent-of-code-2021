module Day08 where

import Util (readDataLines, headMaybe)
import Control.Applicative ((<|>))
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe, isJust)


day08Part1 :: String -> String
day08Part1 s = case readDataLines parseSignalAndOutputStrings s of
    Left e  -> e
    Right x -> show $ sum $ countUniqueTokens <$> x

parseSignalAndOutputStrings :: String -> Maybe (String, String)
parseSignalAndOutputStrings s = case splitOn " | " s of
    [sig, out] -> Just (sig, out)
    _          -> Nothing

parseUnique :: String -> Maybe Int
parseUnique s
    | length s == 2 = Just 1
    | length s == 4 = Just 4
    | length s == 3 = Just 7
    | length s == 7 = Just 8
    | otherwise     = Nothing

countUniqueTokens :: (String, String) -> Int
countUniqueTokens (_, out) = countInString out
    where
        countInString :: String -> Int
        countInString = length . filter isJust . map parseUnique . words

intersection :: String -> String -> String
intersection a b = filter (`elem` b) a

difference :: String -> String -> String
difference a b = filter (not . (`elem` b)) a

isSubset :: String -> String -> Bool
isSubset a b = all (`elem` b) a

-- Zero contains One but not Four
parseZero :: String -> String -> String -> Maybe Int
parseZero oneChars fourChars s
    | length s == 6 && isSubset oneChars s && not (isSubset fourChars s) = Just 0
    | otherwise                                                          = Nothing

-- Two has two overlapping segments with Four
parseTwo :: String -> String -> Maybe Int
parseTwo fourChars s
    | length s == 5 && length (intersection fourChars s) == 2 = Just 2
    | otherwise                                               = Nothing

-- Three is the only 5-segment digit that matches both segments in One
parseThree :: String -> String -> Maybe Int
parseThree oneChars s
    | length s == 5 && isSubset oneChars s = Just 3
    | otherwise                            = Nothing    

-- Five is the only 5-segment digit that matches the two digits in Four that aren't in One
parseFive :: String -> String -> String -> Maybe Int
parseFive oneChars fourChars s
    | length s == 5 && isSubset (difference fourChars oneChars) s = Just 5
    | otherwise                                                   = Nothing

-- Six doesn't contain either One or Four
parseSix :: String -> String -> String -> Maybe Int
parseSix oneChars fourChars s
    | length s == 6 && not (isSubset oneChars s) && not (isSubset fourChars s) = Just 6
    | otherwise                                                                = Nothing

-- Nine is the only 6-segment digit that completely includes Four
parseNine :: String -> String -> Maybe Int
parseNine fourChars s
    | length s == 6 && isSubset fourChars s = Just 9
    | otherwise                             = Nothing

parseDigit :: String -> String -> String -> Maybe Int
parseDigit oneCs fourCs s = 
    parseUnique s <|> parseZero oneCs fourCs s <|> parseTwo fourCs s <|> parseThree oneCs s
    <|> parseFive oneCs fourCs s <|> parseSix oneCs fourCs s <|> parseNine fourCs s

parseDigits :: (String, String) -> Maybe [Int]
parseDigits (s, o) = case (oneChars, fourChars) of
    (Just ones, Just fours) -> mapM (parseDigit ones fours) (words o)
    _                       -> Nothing
    where
        oneChars :: Maybe String
        oneChars = headMaybe $ dropWhile (\x -> length x /= 2) $ words s

        fourChars :: Maybe String
        fourChars = headMaybe $ dropWhile (\x -> length x /= 4) $ words s

digitListToInt :: [Int] -> Int
digitListToInt = read . mconcat . map show

day08Part2 :: String -> String
day08Part2 s = case readDataLines parseSignalAndOutputStrings s of
    Left e   -> e
    Right xs -> maybe "Calculation Failed" (show . sum . map digitListToInt) (mapM parseDigits xs)

    