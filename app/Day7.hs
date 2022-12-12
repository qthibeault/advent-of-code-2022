module Day7 where

data Dir  = Dir String [Node]
data File = File String Int
data Node = F File | D Dir

class FileMeta a where
    size     :: a -> Int
    name     :: a -> String

instance FileMeta Dir where
    size (Dir _ files) = sum $ map size files
    name (Dir n _) = n

instance FileMeta File where
    size (File _ bytes) = bytes
    name (File n _) = n

instance FileMeta Node where
    size (F file) = size file
    size (D dir) = size dir

    name (F file) = name file
    name (D dir) = name dir

showNode :: Int -> Node -> String
showNode indent (F file) = showFile indent file
showNode indent (D dir) = showDir indent dir

showFile :: Int -> File -> String
showFile indent (File name size) = replicate indent ' ' ++ name ++ " (file, size=" ++ show size ++ ")"

showDir :: Int -> Dir -> String
showDir indent (Dir name files) = undefined

mkDirNode :: String -> [Node] -> Node
mkDirNode name files = D (Dir name files)

mkFileNode :: String -> Int -> Node
mkFileNode name size = F (File name size)

dirContents :: Dir -> [Node]
dirContents (Dir _ files) = files

findDirectories :: Int -> Node -> [Dir]
findDirectories _ (F _) = []
findDirectories sizeLimit (D dir)
    | size dir > sizeLimit = []
    | otherwise = D dir : dirContents dir >>= findDirectories sizeLimit

readNode :: [String] -> (Node, [String])
readNode (line:lines) = case words line of
    ["dir", _] -> readNode lines
    ["$", "ls"] -> readNode lines
    [size, name] -> (mkFileNode name (read size), lines)
    ["$", "cd", name] -> let (files, rest) = readNodes lines in (mkDirNode name files, rest)
    _ -> error "Could not node"

readNodes :: [String] -> ([Node], [String])
readNodes = go [] 
    where
        go nodes [] = (nodes, [])
        go nodes ("$ cd ..":lines) = (nodes, lines) 
        go nodes lines = let (node, rest) = readNode lines in go (node:nodes) rest

readRootDir :: [String] -> Dir
readRootDir lines = case readNode lines of
    (D dir, _) -> dir
    _ -> error "Could not read root directory"

readFileLines :: [String] -> Node
readFileLines = D . readRootDir

readFileData :: IO Node 
readFileData = readFileLines . lines <$> readFile "data/day7.txt"

main :: IO ()
main = putStrLn "Day 7"