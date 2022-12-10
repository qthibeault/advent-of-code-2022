module Day6 where

findMessageOffset :: Int -> String -> Int
findMessageOffset prefixLen = go 0 []
    where
        go _ _ [] = error "No prefix found"
        go n prefix (c:rest)
            | length prefix == prefixLen = n
            | otherwise  = let prefix' = takeWhile (c /=) prefix in go (n + 1) (c:prefix') rest

readDatastream :: IO String
readDatastream = head . lines <$> readFile "data/day6.txt"

main :: IO ()
main = do
    datastream <- readDatastream
    print (findMessageOffset 4 datastream)
    print (findMessageOffset 14 datastream)