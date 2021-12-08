module Util where

runDay :: FilePath -> (String -> String) -> (String -> String) -> IO ()
runDay inputFile part1 part2 = do
    fileStr <- readFile inputFile
    putStrLn $ "Part 1: " <> part1 fileStr
    putStrLn $ "Part 2: " <> part2 fileStr

runPart :: FilePath -> (String -> String) -> IO ()
runPart inputFile part = do
    fileStr <- readFile inputFile
    putStrLn $ "Result: " <> part fileStr

readDataLines :: (String -> Maybe a) -> String -> Either String [a]
readDataLines f s = case mapM f (lines s) of
    Just xs -> Right xs
    Nothing -> Left "Error parsing data"

headMaybe :: [a] -> Maybe a
headMaybe []    = Nothing
headMaybe (x:_) = Just x