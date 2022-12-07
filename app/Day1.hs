module Day1 where

splitInventories :: [String] -> [Int] -> [[Int]] -> [[Int]]
splitInventories [] i is = reverse (i:is)
splitInventories ("":lines) i is = splitInventories lines [] (i:is)
splitInventories (line:lines) i is =
    let calories = read line :: Int in splitInventories lines (calories:i) is

readInventories :: IO [[Int]]
readInventories = do
    text <- readFile "data/day1.txt"
    return $ splitInventories (lines text) [] []

maxN' :: Int -> [Int] -> [Int] -> [Int]
maxN' 0 ms xs = ms
maxN' n ms xs = 
    let nthMax = foldl max 0 xs
        xs' = filter (nthMax /=) xs
        ms' = nthMax : ms
    in maxN' (n - 1) ms' xs'

maxN :: Int -> [Int] -> [Int]
maxN n = maxN' n []

main :: IO ()
main = do
    inventories <- readInventories
    let totals = map sum inventories
        maxCalories = foldl max 0 totals
        topThree = maxN 3 totals
    print maxCalories
    print $ sum topThree
