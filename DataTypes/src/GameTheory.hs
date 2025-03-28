module GameTheory where
import DataTypes
import Data.List
-- import Data.Tree (flatten)
-- import Data.ByteString

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

-- Derives difficulty from a list of values
listOfOutcomesDifficulty :: [Value] -> Difficulty
listOfOutcomesDifficulty vs = outcomesDifficulty (length (filter (== W) vs)) (length (filter (== L) vs))

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
gameDifficulty (Node _ Nothing []) = 1.0
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
leastDifficult ds = justToIntOnly (elemIndex (listMin ds) ds)

-- Returns a game's agency. Agency is defined as the order of choices that leads to less than .5 difficulty
gameAgency :: Game -> Agency
gameAgency (Node _ Nothing []) = 1.0
gameAgency (Node p Nothing gs) = if gameDifficulty (Node p Nothing gs) < 0.5 then p else p*gameAgency (gs!!leastDifficult (gamesToDifficulty gs))
gameAgency (Node p _ gs) = p

-- Example: Russian Roullette. Reason being, its easy to implement.

russianRoullette :: Game
russianRoullette = Node 1 Nothing [Node (1.0/6.0) (Just L) [], Node (5.0/6.0) Nothing [Node (1.0/5.0) (Just L) [], Node (4.0/5.0) Nothing [Node (1.0/4.0) (Just L) [], Node (3.0/4.0) Nothing [Node (1.0/3.0) (Just L) [], Node (2.0/3.0) Nothing [Node (1.0/2.0) (Just L) [], Node (1.0/2.0) (Just W) []]]]]]

agencyOfRR :: IO ()
agencyOfRR = print (gameAgency russianRoullette)

-- Game with the map (0, 0)
game_0_0 :: Game
game_0_0 = Node 0 Nothing [Node 0 (Just W) [], Node 0 (Just W) []]

-- Game with the map (1, 0)
game_1_0 :: Game
game_1_0 = makeGame 2 1 0 2

-- Game with the map (0, 1)
game_0_1 :: Game
game_0_1 = makeGame 1 1 1 0

-- Game with the map (1, 1). Not practically possible, only shows up because of a quirk in the definitions.
game_1_1 :: Game
game_1_1 = Node 1 Nothing []

-- Maps a game to their respective f(G)
mapGame :: Game -> (Difficulty, Agency)
mapGame g = (gameDifficulty g, gameAgency g)

printAgencies :: IO()
printAgencies = mapM_ print [mapGame game_0_0, mapGame game_1_0, mapGame game_0_1, mapGame game_1_1]

-- Appends 2 Games to the end of another
appendGames :: Game -> [Game] -> Game
appendGames (Node p v gs) gs1 = Node p v (gs++gs1)

-- Parses string into Parseable (Int or Game).
parseString :: String -> Maybe Parseable
parseString "" = Nothing
parseString s = Just (read s)

parseableToGames :: [Parseable] -> [Game]
parseableToGames ((Int n):ps) = parseableToGames ps
parseableToGames ((Game g):ps) = g:parseableToGames ps
parseableToGames [] = []

maybeToParseable :: Maybe Parseable -> Parseable
maybeToParseable (Just p) = p
maybeToParseable Nothing = Int 0

maybeGameToGame :: Maybe Game -> Game
maybeGameToGame (Just g) = g
maybeGameToGame Nothing = Node 1 Nothing []

parsedToGames :: [Maybe Parseable] -> [Game]
parsedToGames ((Just (Int n)):ps) = appendGames (Node 1 Nothing []) (parseableToGames (map maybeToParseable (take n ps))):parsedToGames ps
parsedToGames ((Just (Game g)):ps) = parsedToGames ps
parsedToGames (Nothing:ps) = parsedToGames ps
parsedToGames [] = []


parseGames :: [String] -> [Game]
parseGames ss = parsedToGames (map parseString ss)

parseGamesAndJoin :: [String] -> Game
parseGamesAndJoin ss = appendGames (Node 1 Nothing []) (parseGames ss)


-- main :: IO()
-- main = printAgencies