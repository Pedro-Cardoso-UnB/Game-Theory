module DataTypes where

type Probability = Float
data Value = W | L deriving(Show, Eq, Read)

-- In this, a Game is defined as a tree with n other games connected to it. The Probability is the chance of arriving in that game from the previous node in the tree.
data Game = Node Probability (Maybe Value) [Game] deriving(Show, Eq, Read)

type Difficulty = Float 
type Agency = Float

-- Needed to parse files into games
data Parseable = Int Int | Game Game deriving(Read)
