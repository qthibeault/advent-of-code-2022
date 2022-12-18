module Day8 where

import Data.Char (digitToInt)
import Data.Foldable (maximum)
import Data.List (transpose)

newtype Tree = Tree Int deriving (Eq, Ord, Show)
newtype TreeGrid = TreeGrid [[Tree]]

readTreeGrid :: [String] -> TreeGrid
readTreeGrid rows =
    let readTreeRow = map (Tree . digitToInt)
        trees = map readTreeRow rows
     in TreeGrid trees

readGridFile :: IO TreeGrid
readGridFile = readTreeGrid . lines <$> readFile "data/day8.txt"

gridWidth :: [[a]] -> Int
gridWidth [] = error "Empty grid has no width"
gridWidth (xs : xss) = length xs

treesAboveBelow :: (Int, Int) -> [[Tree]] -> ([Tree], [Tree])
treesAboveBelow (row, col) grid =
    let (rowsAbove, rowsBelow) = splitAt row grid
     in ([row !! col | row <- rowsAbove], [row !! col | row <- tail rowsBelow])

treesLeftRight :: (Int, Int) -> [[Tree]] -> ([Tree], [Tree])
treesLeftRight (row, col) grid =
    let columns = grid !! row
        (columnsLeft, columnsRight) = splitAt col columns
     in (columnsLeft, tail columnsRight)

isVisible :: [Tree] -> [Tree] -> [Tree] -> [Tree] -> Tree -> Bool
isVisible [] _ _ _ _ = True
isVisible _ [] _ _ _ = True
isVisible _ _ [] _ _ = True
isVisible _ _ _ [] _ = True
isVisible above right below left tree = all (tree >) above || all (tree >) below || all (tree >) left || all (tree >) right

visibleTrees :: TreeGrid -> [Tree]
visibleTrees (TreeGrid grid) = [(grid !! r) !! c | r <- rowIdxs, c <- colIdxs, idxVisible r c]
  where
    rowIdxs = [0 .. (length grid - 1)]
    colIdxs = [0 .. (gridWidth grid - 1)]
    idxVisible r c =
        let tree = (grid !! r) !! c
            (above, below) = treesAboveBelow (r, c) grid
            (left, right) = treesLeftRight (r, c) grid
         in isVisible above right below left tree

countVisibleFrom :: Tree -> [Tree] -> Int
countVisibleFrom _ [] = 0
countVisibleFrom  source (tree:trees)
    | tree >= source = 1
    | otherwise = 1 + countVisibleFrom source trees

treeScore :: [[Tree]] -> Tree -> Int
treeScore directions tree
    | length directions /= 4 = error "Must provide 4 tree directions"
    | otherwise =
        let counts = map (countVisibleFrom tree) directions
        in product counts

treeScores :: TreeGrid -> [Int]
treeScores (TreeGrid grid) = [scoreTree r c | r <- rowIdxs, c <- colIdxs]
    where
        rowIdxs = [0 .. (length grid - 1)]
        colIdxs = [0 .. (gridWidth grid - 1)]
        scoreTree r c =
            let tree = (grid !! r) !! c
                (above, below) = treesAboveBelow (r, c) grid
                (left, right) = treesLeftRight (r, c) grid
            in treeScore [reverse above, right, below, reverse left] tree

main :: IO ()
main = do
    grid <- readGridFile
    print $ length $ visibleTrees grid
    print $ maximum $ treeScores grid
