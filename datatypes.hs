module DataTypes where

type Probability = Float
data Value = W | L deriving(Show, Eq)

-- In this, a Game is defined as a tree with n other games connected to it. The Probability is the chance of arriving in that game from the previous node in the tree.
data Game = Node Probability (Maybe Value) [Game] deriving(Show, Eq)

type Difficulty = Float 
type Agency = Float
