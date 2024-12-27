import DataTypes
import Data.ByteString (elemIndex)

-- randomProbability :: Int -> Probability
-- randomProbability n = ((27*n + 128) `rem` (100))/100

-- nroot :: Int -> Int -> Float
-- nroot o n = o**(1/n)

-- Makes the delta of a game graph. Takes 2 integers, which are the number of W and L outcomes respectively. Returns a game
makeGameDelta :: Int -> Int -> Game
makeGameDelta x y = Node (1/(w+l)) Nothing (replicate x (Node (1/(w+l)) (Just W) []) ++ replicate y (Node (1/(w+l)) (Just L) []))
    where
        w = fromIntegral x :: Float
        l = fromIntegral y :: Float

-- Takes n games and unites them under a main one
uniteGames :: [Game] -> Game
uniteGames gs = Node (1.0/x) Nothing gs
    where x = fromIntegral (length gs) :: Float

-- Takes depth, number of choices per game state, number of wins, number of losses, and outputs a game
makeGame :: Int -> Int -> Int -> Int -> Game
makeGame _ 0 _ _ = Node 1 Nothing []
makeGame _ 1 x y = makeGameDelta x y
makeGame z n x y = uniteGames (replicate z (makeGame z (n - 1) (floor (w/c)) (floor (l/c))))
    where
        w = fromIntegral x :: Float
        l = fromIntegral y :: Float
        c = fromIntegral z :: Float

-- Returns difficulty given outcomes
outcomesDifficulty :: Int -> Int -> Difficulty
outcomesDifficulty wi li = l/(l + w)
    where
        w = fromIntegral wi :: Float
        l = fromIntegral li :: Float

-- Derives odifficulty from a list of values
listOfOutcomesDifficulty :: [Value] -> Difficulty
listOfOutcomesDifficulty vs = outcomesDifficulty (length (filter (== L) vs)) (length (filter (== W) vs))

-- Flattens a game tree into an array of games with empty subgames
flattenGame :: Game -> [Game]
flattenGame (Node p v gs) = Node p v [] : concatMap flattenGame gs

-- Similar to flattenGame, lists all the non-Nothing values in a list of games.
gamesToValues :: [Game] -> [Value]
gamesToValues ((Node _ (Just v) _):gs) = v:gamesToValues gs
gamesToValues ((Node _ Nothing _):gs) = gamesToValues gs
gamesToValues [] = []

-- Decomposes game tree into only its final outcomes
listOutcomes :: Game -> [Value]
listOutcomes g = gamesToValues (flattenGame g)

-- Returns difficulty given game
gameDifficulty :: Game -> Difficulty
gameDifficulty g = listOfOutcomesDifficulty (listOutcomes g)

-- Gets the probability from a game
getProbability :: Game -> Probability
getProbability (Node p _ _) = p

-- Turns Maybe Int into Int
justToIntOnly :: (Integral a) => Maybe a -> a
justToIntOnly (Just a) = a
justToIntOnly Nothing  = -1

-- Iterative min for lists
listMin :: Ord a => [a] -> a
listMin (a1:a2:as) = if a1 < a2 then listMin (a1:as) else listMin (a2:as)
listMin [a] = a

-- Converts a list of games into a list of their difficulties
gamesToDifficulty :: [Game] -> [Difficulty]
gamesToDifficulty = map gameDifficulty

-- Index of an element in a list
indexOf :: (Eq a) => a -> [a] -> Int
indexOf n xs = go 0 n xs
    where
        go i n [] = -1
        go i n (x:xs)
             | n == x    = i
             | otherwise = go (i+1) n xs

-- Results in the index of the smallest difficulty in a list
leastDifficult :: [Difficulty] -> Int
leastDifficult ds = indexOf (listMin ds) ds

-- Returns a game's agency
gameAgency :: Game -> Agency
gameAgency (Node p v gs) = if gameDifficulty (gs!!leastDifficult (gamesToDifficulty gs)) < 0.5 then p else p*gameAgency (gs!!leastDifficult (gamesToDifficulty gs))

main :: IO ()
main = print (gameAgency (makeGame 2 2 2 2))
-- main = print (makeGame 2 2 2 2)