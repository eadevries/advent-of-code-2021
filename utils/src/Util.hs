module Util where

readDataLines :: (String -> Maybe a) -> String -> Either String [a]
readDataLines f s = case mapM f (lines s) of
    Just xs -> Right xs
    Nothing -> Left "Error parsing data"