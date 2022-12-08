module Day2 where

data Choice = Rock | Paper | Scissors deriving (Eq)
data Outcome = Win | Loss | Tie deriving (Eq)

instance Read Choice where
    readsPrec _ [] = []
    readsPrec _ (choice:rest)
        | choice `elem` ['A', 'X'] = [(Rock, rest)]
        | choice `elem` ['B', 'Y'] = [(Paper, rest)]
        | choice `elem` ['C', 'Z'] = [(Scissors, rest)]
        | otherwise = []

class Scored a where
    score :: a -> Int

instance Scored Choice where
    score Rock = 1
    score Paper = 2
    score Scissors = 3

instance Scored Outcome where
    score Win = 6
    score Tie = 3
    score Loss = 0

-- Compute the outcome of a game. The opponent's choice is first, the player's choice is second.
gameOutcome :: (Choice, Choice) -> Outcome
gameOutcome (Rock, Paper) = Win
gameOutcome (Rock, Scissors) = Loss
gameOutcome (Paper, Scissors) = Win
gameOutcome (Paper, Rock) = Loss
gameOutcome (Scissors, Rock) = Win
gameOutcome (Scissors, Paper) = Loss
gameOutcome _ = Tie

gameScore :: (Choice, Choice) -> Int
gameScore game@(_, choice) =
    let outcome = gameOutcome game in score outcome + score choice

readGame :: String -> (Choice, Choice)
readGame line =
    let left:right:_ = words line in (read left, read right)

readGames :: String -> [(Choice, Choice)]
readGames = map readGame . lines

-- Return the move that will lose to the given move
losingChoice :: Choice -> Choice
losingChoice Rock = Scissors
losingChoice Paper = Rock
losingChoice Scissors = Paper

-- Return the move that will tie the given move
tyingChoice :: Choice -> Choice
tyingChoice = id

-- Return the move that will beat the given move
winningChoice :: Choice -> Choice
winningChoice Rock = Paper
winningChoice Paper = Scissors
winningChoice Scissors = Rock

strategyChoice :: String -> Choice -> Choice
strategyChoice "X" = losingChoice
strategyChoice "Y" = tyingChoice
strategyChoice "Z" = winningChoice

readGame' :: String -> (Choice, Choice)
readGame' line =
    let left:right:_ = words line
        choice = read left :: Choice
    in (choice, strategyChoice right choice)

readGames' :: String -> [(Choice, Choice)]
readGames' = map readGame' . lines

main :: IO ()
main = do
    games <- readGames <$> readFile "data/day2.txt"
    print (sum $ map gameScore games)
    games' <- readGames' <$> readFile "data/day2.txt"
    print (sum $ map gameScore games')
