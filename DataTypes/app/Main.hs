import GameTheory
import DataTypes
import System.IO
    ( hGetContents, hClose, IOMode(ReadWriteMode), openFile )
import Data.Char ( toUpper )
import Control.Monad (when, unless)



main :: IO ()
main = do
    contents <- readFile "game.txt"
    let mappedGames :: [(Difficulty, Agency)]
        mappedGames = map mapGame (parseGames (lines contents))
    let mapSingleGame :: (Difficulty, Agency)
        mapSingleGame = mapGame (parseGamesAndJoin (lines contents))
    let game :: Game
        game = parseGamesAndJoin (lines contents)
    unless (null mappedGames) $
        writeFile "game_map.txt" (show mapSingleGame ++ "\n" ++ show game)
