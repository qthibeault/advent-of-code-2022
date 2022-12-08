module Day4 where

import Data.Bifunctor (bimap, second)
import Data.List (elemIndex, splitAt)

newtype Interval = Interval (Int, Int)

splitOn :: Char ->  String -> (String, String)
splitOn c s = case elemIndex c s of
    Just i -> second (drop 1) $ splitAt i s
    Nothing -> (s, [])

readRange :: String -> (Int, Int)
readRange = bimap read read . splitOn '-'

readRanges :: String -> ((Int, Int), (Int, Int))
readRanges = bimap readRange readRange . splitOn ','

rangeContains :: (Int, Int) -> Int -> Bool
rangeContains (l, h) i
    | l <= i && i <= h = True
    | otherwise = False

rangeFullyContains :: (Int, Int) -> (Int, Int) -> Bool
rangeFullyContains r1@(l1, h1) r2@(l2, h2) = r1Contains || r2Contains
    where
        r1Contains = rangeContains r1 l2 && rangeContains r1 h2
        r2Contains = rangeContains r2 l1 && rangeContains r2 h1

rangesOverlap :: (Int, Int) -> (Int, Int) -> Bool
rangesOverlap r1@(l1, h1) r2@(l2, h2) =
    rangeContains r1 l2 || rangeContains r1 h2 || rangeContains r2 l1 || rangeContains r2 h1

main :: IO ()
main = do
    ranges <- map readRanges . lines <$> readFile "data/day4.txt"
    print (length $ filter (uncurry rangeFullyContains) ranges)
    print (length $ filter (uncurry rangesOverlap) ranges)
