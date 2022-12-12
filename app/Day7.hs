module Day7 where

import Data.Foldable (foldl', minimumBy)
import Data.List (intercalate)
import Text.Printf (printf)

data Dir  = Dir String [Node] deriving (Show)
data File = File String Int deriving (Show)
data Node = F File | D Dir deriving (Show)

instance Eq Dir where
    (Dir n1 _) == (Dir n2 _) = n1 == n2

instance Ord Dir where
    compare d1 d2 = compare (size d1) (size d2) 

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
showFile indent (File name size) = printf "%s- %s (file, size=%d)\n" (replicate indent ' ') name size

showDir :: Int -> Dir -> String
showDir indent (Dir name files) = 
    let firstLine = printf "%s- %s (dir)\n" (replicate indent ' ') name
        contentLines = map (showNode (indent + 2)) files
    in firstLine ++ concat contentLines

mkDirNode :: String -> [Node] -> Node
mkDirNode name files = D (Dir name files)

mkFileNode :: String -> Int -> Node
mkFileNode name size = F (File name size)

readNode :: [String] -> (Node, [String])
readNode (line:lines) = case words line of
    ["dir", _] -> readNode lines
    ["$", "ls"] -> readNode lines
    [size, name] -> (mkFileNode name (read size), lines)
    ["$", "cd", name] -> let (files, rest) = readDirContents lines in (mkDirNode name files, rest)
    _ -> error "Could not node"

readDirContents :: [String] -> ([Node], [String])
readDirContents = go [] 
    where
        go nodes [] = (nodes, [])
        go nodes ("$ cd ..":lines) = (nodes, lines) 
        go nodes lines = let (node, rest) = readNode lines in go (node:nodes) rest

readRootDir :: [String] -> Dir
readRootDir lines = case readNode lines of
    (D dir, _) -> dir
    _ -> error "Could not read root directory"

readRootNode :: IO Node 
readRootNode = fst . readNode . lines <$> readFile "data/day7.txt"

dirContents :: Dir -> [Node]
dirContents (Dir _ files) = files

findDirs :: (Dir -> Bool) -> Node -> [Dir]
findDirs _ (F _) = []
findDirs f (D dir)
    | f dir = dir : includedSubdirs
    | otherwise = includedSubdirs
    where
        includedSubdirs = dirContents dir >>= findDirs f

findDirsSmallerThan :: Int -> Node -> [Dir]
findDirsSmallerThan upper = findDirs (\d -> size d <= upper)

findDirsLargerThan :: Int -> Node -> [Dir]
findDirsLargerThan lower = findDirs (\d -> size d >= lower)

diskCapacity :: Int
diskCapacity = 70000000

spaceRequired :: Int
spaceRequired = 30000000

main :: IO ()
main = do
    rootNode <- readRootNode
    print $ foldl' (\acc elem -> acc + size elem) 0 (findDirsSmallerThan 100000 rootNode)
    let spaceToFree = spaceRequired - (diskCapacity - size rootNode)
    print $ size $ minimum (findDirsLargerThan spaceToFree rootNode)
    