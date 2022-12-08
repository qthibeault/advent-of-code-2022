module Day5 where

import Data.Bifunctor (bimap)
import Data.List (elemIndex, splitAt)
import Data.Maybe (fromJust)

type Stack = String
data Move = Move Int Int Int deriving (Show)

readCrate :: String -> String
readCrate ('[':i:']':_) = [i]
readCrate _ = ""

columns :: Int -> String -> [String]
columns 0 _ = []
columns n line
    | null line = replicate n ""
    | otherwise = take 4 line : columns (n - 1) (drop 4 line)

prependRowToStacks :: String -> [Stack] -> [Stack]
prependRowToStacks l ss =
    let cs = map readCrate $ columns (length ss) l
    in zipWith (++) cs ss

stacksFromLines :: [String] -> [Stack]
stacksFromLines lines =
    let nStacks = length $ words $ last lines
        stacks = replicate nStacks ""
    in foldr prependRowToStacks stacks (init lines)

moveFromLine :: String -> Move
moveFromLine line =
    let parts = words line
        n = read $ parts !! 1
        from = read $ parts !! 3
        to = read $ parts !! 5
    in Move n (from - 1) (to - 1)

movesFromLines :: [String] -> [Move]
movesFromLines = map moveFromLine

readProblem :: String -> ([Stack], [Move])
readProblem text = 
    let textLines = lines text
        splitIndex = fromJust $ elemIndex "" textLines
    in bimap stacksFromLines (movesFromLines . tail) $ splitAt splitIndex textLines

replaceAt :: Int -> a -> [a] -> [a]
replaceAt idx x xs
    | idx < 0 = error "Index must be greater than zero"
    | idx >= length xs = error "Index must be less than the length of the list"
    | otherwise =
        let (left, _:right) = splitAt idx xs in left ++ x:right

topNItemsFromStack :: Int -> Int -> [Stack] -> [Char]
topNItemsFromStack idx n ss = take n (ss !! idx)

removeNItemsFromStack :: Int -> Int -> [Stack] -> [Stack]
removeNItemsFromStack idx n ss = let s = ss !! idx in replaceAt idx (drop n s) ss

addItemsToStack :: Int -> Stack -> [Stack] -> [Stack]
addItemsToStack idx s ss = let s' = ss !! idx in replaceAt idx (s ++ s') ss

moveResult :: Move -> [Stack] -> [Stack]
moveResult (Move n from to) ss = 
    let items = reverse (topNItemsFromStack from n ss)
    in addItemsToStack to items (removeNItemsFromStack from n ss)

movesResult :: [Move] -> [Stack] -> [Stack]
movesResult [] = id
movesResult (m:ms) = movesResult ms . moveResult m

moveResult' :: Move -> [Stack] -> [Stack]
moveResult' (Move n from to) ss = 
    let items = topNItemsFromStack from n ss
    in addItemsToStack to items (removeNItemsFromStack from n ss)

movesResult' :: [Move] -> [Stack] -> [Stack]
movesResult' [] = id
movesResult' (m:ms) = movesResult' ms . moveResult' m

topItems :: [Stack] -> String
topItems = map head 

main :: IO ()
main = do
    (stacks, moves) <- readProblem <$> readFile "data/day5.txt"
    print (topItems $ movesResult moves stacks)
    print (topItems $ movesResult' moves stacks)
