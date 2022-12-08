module Day3 where

import Data.Char (isLower, isUpper, toLower)
import Data.List (elemIndex, find, nub, splitAt)
import Data.Maybe (fromJust)

type Item = Char

splitCompartments :: [Item] -> ([Item], [Item])
splitCompartments cs = splitAt (length cs `div` 2) cs

findDuplicates :: ([Item], [Item]) -> [Item]
findDuplicates (left, right) = [c | c <- left, c `elem` right]

prioritizeItem :: Item -> Int
prioritizeItem x
    | isLower x = 1 + fromJust (elemIndex x ['a'..'z'])
    | isUpper x = 26 + prioritizeItem (toLower x)
    | otherwise = error "Item must be in [a-zA-Z]"

scoreRucksacks :: [[Item]] -> [Int]
scoreRucksacks = map process
    where process = sum . map prioritizeItem . nub . findDuplicates . splitCompartments

groupRucksacks :: Int -> [[Item]] -> [[[Item]]]
groupRucksacks _ [] = []
groupRucksacks n xs = take n xs : groupRucksacks n (drop n xs)

findGroupItem :: [[Item]] -> Item
findGroupItem [] = error "No rucksacks in group"
findGroupItem [r1] = error "Must have more than 1 rucksack in a group"
findGroupItem (r1:rs) =
    let duplicate = find (\i -> all (\r -> i `elem` r) rs) r1
    in case duplicate of
        Just d -> d
        Nothing -> error "No duplicate in rucksacks"

scoreRucksackGroups :: [[Item]] -> [Int]
scoreRucksackGroups rs = map (prioritizeItem . findGroupItem) $ groupRucksacks 3 rs

readRucksacks :: IO [[Item]]
readRucksacks = lines <$> readFile "data/day3.txt"

main :: IO ()
main = do
    rucksacks <- readRucksacks
    print $ sum (scoreRucksacks rucksacks)
    print $ sum (scoreRucksackGroups rucksacks)