module Ledger.Halifax.Utilities where

import Data.Char
import Data.Maybe


trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

fragment :: (a -> Bool) -> (a -> Bool) -> [a] -> ([a], [a])
fragment start end = span (not . end) . dropWhile (not . start)

firstJust :: [Maybe a] -> Maybe a
firstJust = listToMaybe . catMaybes

notNull :: [a] -> Bool
notNull = not . null

onLeft :: (a -> c) -> (a, b) -> (c, b)
onLeft f (x, y) = (f x, y)

onRight :: (b -> c) -> (a, b) -> (a, c)
onRight f (x, y) = (x, f y)

splitEithers :: [Either a b] -> ([a], [b])
splitEithers [] = ([], [])
splitEithers (Left x:rest) = onLeft (x:) $ splitEithers rest
splitEithers (Right y:rest) = onRight (y:) $ splitEithers rest